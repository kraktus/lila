import { h, type VNode } from 'snabbdom';
import { json as xhrJson, form as xhrForm } from 'lib/xhr';
import { bind } from 'lib/snabbdom';
import type { Convo, Msg, Daily } from '../interfaces';
import { enhance, isMoreThanText, expandLpvs } from './enhance';
import { scroller } from './scroller';
import type MsgCtrl from '../ctrl';
import { alert, confirm, makeLinkPopups } from 'lib/view/dialogs';

export default function renderMsgs(ctrl: MsgCtrl, convo: Convo): VNode {
  return h('div.msg-app__convo__msgs', { hook: { insert: setupMsgs(true), postpatch: setupMsgs(false) } }, [
    h('div.msg-app__convo__msgs__init'),
    h('div.msg-app__convo__msgs__content', [
      ctrl.canGetMoreSince
        ? h(
            'button.msg-app__convo__msgs__more.button.button-empty',
            {
              key: 'more',
              attrs: { type: 'button' },
              hook: bind('click', _ => {
                scroller.setMarker();
                ctrl.getMore();
              }),
            },
            'Load more',
          )
        : null,
      ...contentMsgs(ctrl, convo.msgs),
      h('div.msg-app__convo__msgs__typing', ctrl.typing ? `${convo.user.name} is typing...` : null),
    ]),
  ]);
}

function contentMsgs(ctrl: MsgCtrl, msgs: Msg[]): VNode[] {
  const dailies = groupMsgs(msgs);
  const nodes: VNode[] = [];
  dailies.forEach(daily => nodes.push(...renderDaily(ctrl, daily)));
  return nodes;
}

function renderDaily(ctrl: MsgCtrl, daily: Daily): VNode[] {
  return [
    h('day', renderDate(daily.date)),
    ...daily.msgs.map(group =>
      h(
        'group',
        group.map(msg => renderMsg(ctrl, msg)),
      ),
    ),
  ];
}

function renderMsg(ctrl: MsgCtrl, msg: Msg) {
  const tag = msg.user === ctrl.data.me.id ? 'mine' : 'their';
  return h(tag, [renderText(msg), h('em', `${pad2(msg.date.getHours())}:${pad2(msg.date.getMinutes())}`)]);
}
const pad2 = (num: number): string => (num < 10 ? '0' : '') + num;

function groupMsgs(msgs: Msg[]): Daily[] {
  let prev: Msg = msgs[0];
  if (!prev) return [{ date: new Date(), msgs: [] }];
  const dailies: Daily[] = [{ date: prev.date, msgs: [[prev]] }];
  msgs.slice(1).forEach(msg => {
    if (sameDay(msg.date, prev.date)) {
      if (msg.user === prev.user) dailies[0].msgs[0].unshift(msg);
      else dailies[0].msgs.unshift([msg]);
    } else dailies.unshift({ date: msg.date, msgs: [[msg]] });
    prev = msg;
  });
  return dailies;
}

const today = new Date();
const yesterday = new Date();
yesterday.setDate(yesterday.getDate() - 1);

function renderDate(date: Date) {
  if (sameDay(date, today)) return i18n.site.today.toUpperCase();
  if (sameDay(date, yesterday)) return i18n.site.yesterday.toUpperCase();
  return renderFullDate(date);
}

const renderFullDate = (date: Date) => {
  // Display date using locale culture settings.
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString
  // For example: 2023-03-15
  // de-DE: "15.3.2023"
  // us-US: "3/15/2023"
  const options = { year: 'numeric', month: 'numeric', day: 'numeric' } as const;
  return date.toLocaleDateString(undefined, options);
};

const sameDay = (d: Date, e: Date) =>
  d.getDate() === e.getDate() && d.getMonth() === e.getMonth() && d.getFullYear() === e.getFullYear();

const renderText = (msg: Msg) =>
  isMoreThanText(msg.text)
    ? h('t', {
        hook: {
          create(_, vnode: VNode) {
            const el = vnode.elm as HTMLElement;
            el.innerHTML = enhance(msg.text);
            el.querySelectorAll('img').forEach(c =>
              c.addEventListener('load', scroller.auto, { once: true }),
            );
            $('form.unsub', el).on('submit', function (this: HTMLFormElement) {
              teamUnsub(this);
              return false;
            });
          },
        },
      })
    : h('t', msg.text);

const setupMsgs = (insert: boolean) => (vnode: VNode) => {
  const el = vnode.elm as HTMLElement;
  if (insert) scroller.init(el);
  expandLpvs(el);
  makeLinkPopups(el, 'their a[href^="http"]');
  scroller.toMarker() || scroller.auto();
};

const teamUnsub = async (form: HTMLFormElement) => {
  if (await confirm('Unsubscribe?'))
    xhrJson(form.action, {
      method: 'post',
      body: xhrForm({ subscribe: false }),
    }).then(() => alert('Done!'));
};
