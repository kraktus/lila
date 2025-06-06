import { h } from 'snabbdom';
import { onInsert } from 'lib/snabbdom';
import { renderClock } from 'lib/view/miniBoard';
import type SimulCtrl from '../ctrl';
import type { Pairing } from '../interfaces';
import { opposite } from '@lichess-org/chessground/util';
import { userFlair } from 'lib/view/userLink';

export default function (ctrl: SimulCtrl) {
  return h('div.game-list.now-playing.box__pad', ctrl.data.pairings.map(miniPairing(ctrl)));
}

const miniPairing = (ctrl: SimulCtrl) => (pairing: Pairing) => {
  const game = pairing.game,
    player = pairing.player,
    flair = userFlair(player);
  return h(
    `span.mini-game.mini-game-${game.id}.mini-game--init.is2d`,
    {
      class: { host: ctrl.data.host.gameId === game.id },
      attrs: {
        'data-state': `${game.fen},${game.orient},${game.lastMove}`,
        'data-live': game.clock ? game.id : '',
      },
      hook: onInsert(site.powertip.manualUserIn),
    },
    [
      h('span.mini-game__player', [
        h('a.mini-game__user.ulpt', { attrs: { href: `/@/${player.name}` } }, [
          h(
            'span.name',
            player.title ? [h('span.utitle', player.title), ' ', player.name, flair] : [player.name, flair],
          ),
          ...(ctrl.opts.showRatings ? [' ', h('span.rating', player.rating)] : []),
        ]),
        game.clock
          ? renderClock(opposite(game.orient), game.clock[opposite(game.orient)])
          : h('span.mini-game__result', game.winner ? (game.winner === game.orient ? 0 : 1) : '½'),
      ]),
      h('a.cg-wrap', { attrs: { href: `/${game.id}/${game.orient}` } }),
      h('span.mini-game__player', [
        h('span'),
        game.clock
          ? renderClock(game.orient, game.clock[game.orient])
          : h('span.mini-game__result', game.winner ? (game.winner === game.orient ? 1 : 0) : '½'),
      ]),
    ],
  );
};
