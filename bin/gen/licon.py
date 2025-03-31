#!/usr/bin/env python3
import subprocess
import re
import textwrap
import os
import sys
import platform
from os.path import *
import argparse
import tempfile

comment_preamble = """/*
 * This source is generated by bin/gen/licon.py. Run licon.py after changing public/font/lichess.sfd
 *
 * Constant names and values are pulled from private use characters defined in the sfd file.
 *
 * Character names can be edited in fontforge's "glyph info" dialog, or by editing the StartChar: line that
 * begins each character chunk in lichess.sfd
 *
 * To make these characters visible in your editor, install the lichess.ttf font (which is also generated by
 * licon.py) and then add it to your editor's font list.
 */
"""

scala_preamble = comment_preamble + """
package lila.ui

opaque type Icon = String
object Icon:
  def apply(char: String): Icon            = char
  extension (icon: Icon) def value: String = icon
  given scalalib.Iso.StringIso[Icon]       = scalalib.Iso.string(apply, value)
  import play.api.libs.json.{ Writes, JsString }
  given Writes[Icon]        = JsString(_)
"""

debug_preamble = """<!--""" + comment_preamble + """-->
<!doctype html>
<html lang="en">
  <head>
    <style>
      @font-face {
        font-family: 'lichess';
        src: url('../font/lichess.woff2') format('woff2');
        font-display: block;
        font-weight: normal;
        font-style: normal;
      }
      body {
        font-family: lichess;
        font-size: 82px;
        display: flex;
        flex-flow: row wrap;
        align-content: flex-start;
      }
      i {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 1.2em;
        height: 1.2em;
        font-style: normal;
        color: #333;
        outline: 1px solid #555;
      }
    </style>
  </head>
  <body>"""

def main():
    parser = argparse.ArgumentParser(description="""
        licon.py uses public/font/lichess.sfd and fontforge to build fonts and update lila code. Use --check or --replace
        to check for or fix any embedded licon literals in your sources""")
    parser.add_argument('--check', action='store_true', help='report any embedded licon literals in your sources')

    args = parser.parse_args()
    if '--help' in sys.argv or '-h' in sys.argv:
        parser.print_help()
        return

    lila_chdir('public/font')
    codes = parse_codes()
    
    if args.check:
        lila_chdir()
        sys.exit(check_sources({chr(v): k for k, v in codes.items()}))
    else:

        gen_fonts()

        gen_sources(codes)

        print('Generated:\n  public/font/lichess.woff2\n  public/font/lichess.ttf\n  public/oops/font.html')
        print('  modules/ui/src/main/Icon.scala\n  ui/lib/src/licon.ts')
        print('  ui/lib/css/abstract/_licon.scss\n')
        print("Don't forget to install lichess.ttf in your code editor\n")


def lila_chdir(s = '', lila_root = abspath(join(dirname(__file__), '../../'))):
    os.chdir(join(lila_root, s))


def dash_camel(s):
    return ''.join([w.title() for w in s.split('-')])

def parse_codes():
    unnamed_re = re.compile(r'$|uni[a-f0-9]{4}', re.IGNORECASE)
    codes = {}
    warnings = []
    used = set()
    with open('lichess.sfd', 'r') as f:
        lines = f.readlines()
        name = None
        for line in lines:
            if line.startswith('StartChar:'):
                name = dash_camel(line.split(': ')[1].strip())
            elif line.startswith('Encoding:') and name is not None:
                code_point = int(line.split(' ')[1])
                if code_point >= 0xe000 and code_point <= 0xefff:
                    if unnamed_re.match(name):
                        warnings.append(f'  Unnamed glyph "{name}" at code point {code_point}\n')
                        continue
                    if code_point in used:
                        warnings.append(f'  "{name}" has duplicate code point {code_point}')
                        continue
                    used.add(code_point)
                    codes[name] = code_point
    print('' if not warnings else f'\nWarnings:\n{"".join(warnings)}')
    return dict(sorted(codes.items(), key=lambda x: x[1]))


def gen_sources(codes):
    with_type = lambda name: f'{name}: Icon'
    longest = len(max(codes.keys(), key=len)) + 6

    with open('../../modules/ui/src/main/Icon.scala', 'w') as scala, \
         open('../../ui/lib/src/licon.ts', 'w') as ts, \
         open('../../ui/lib/css/abstract/_licon.scss', 'w') as scss, \
         open('../../public/oops/font.html', 'w') as debug:
        scala.write(scala_preamble)
        ts.write(comment_preamble + '\n')
        scss.write(comment_preamble + '\n')
        debug.write(debug_preamble + '\n')
        for name in codes:
            scala.write(f'  val {with_type(name).ljust(longest)} = "{chr(codes[name])}" // {codes[name]:x}\n')
            ts.write(f"export const {name} = '{chr(codes[name])}'; // {codes[name]:x}\n")
            scss.write(f"$licon-{name}: '{chr(codes[name])}'; // {codes[name]:x}\n")
            debug.write(f'    <i title="{name}">&#x{codes[name]:x};</i>\n')
        debug.write('  </body>\n</html>\n')

def gen_fonts():
    [f, name] = tempfile.mkstemp(suffix='.pe', dir='.')
    os.write(f, textwrap.dedent(f"""
        Open('lichess.sfd')
        Generate('lichess.woff2')
        Generate('lichess.ttf')
        Quit()
    """).encode('utf-8'))

    if platform.system() == 'Darwin':
        exepath = '/Applications/FontForge.app/Contents/Resources/opt/local/bin/fontforge'
    else:
        exepath = 'fontforge'
    subprocess.run([exepath, '-script', name])
    os.remove(name)


def check_sources(names):
    search_re = re.compile(u'[\ue000-\uefff]')
    search_cp_re = re.compile(r'\\u(e[0-9a-f]{3})', re.IGNORECASE)
    sources = []

    for dir, _, files in os.walk('.'):
        if any(skip in dir for skip in ('/node_modules', '/dist', '.metals', '/compiled')):
            continue
        sources.extend([join(dir, f) for f in filter(
            lambda f: \
                any(map(lambda e: f.endswith(e), ['.ts', '.js', '.mts', '.mjs', '.scala', '.scss'])) \
                and not f in ['Icon.scala', 'licon.ts', '_licon.scss'],
            files
        )])

    bad = 0
    for source in sources:
        with open(source, 'r') as f:
            text = f.read()
            for regex in [search_re, search_cp_re]:
                m = regex.search(text)
                while m is not None:
                    bad += 1
                    line = text[:m.start()].count('\n') + 1
                    ch = m.group(0) if regex == search_re else chr(int(m.group(1),16))
                    sub = "(unknown)"
                    if ch in names:
                        if source.endswith('.scss'):
                            sub = f'($licon-{names[ch]})'
                        elif source.endswith('.scala'):
                            sub = f'(Icon.{names[ch]})'
                        elif source.endswith('.ts'):
                            sub = f'(licon.{names[ch]})'
                        else:
                            sub = f'({names[ch]})'
                    print(f"  {source}:{line} found '{m.group(0)}' {sub}")
                    m = regex.search(text, m.end())
    if bad == 0:
        print('None found')
    else:
        print(f'{bad} found')
    return bad

if __name__ == "__main__":
    main()
