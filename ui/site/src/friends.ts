import { notNull } from 'lib';
import * as licon from 'lib/licon';
import { api as lichess } from 'lib/api';

type TitleName = string;

interface Friend {
  id: string;
  name: string;
  title?: string;
  playing: boolean;
  patron: boolean;
}

export default class OnlineFriends {
  titleEl: HTMLElement;
  loaded = false;
  users: Map<string, Friend>;

  constructor(readonly el: HTMLElement) {
    const api = lichess.onlineFriends;
    this.titleEl = this.el.querySelector('.friend_box_title') as HTMLElement;
    this.titleEl.addEventListener('click', () => {
      this.el.querySelector('.content_wrap')?.classList.toggle('none');
      if (!this.loaded) {
        this.loaded = true;
        api.request();
      }
    });
    this.users = new Map();
    api.events.on('onlines', this.receive);
    api.events.on('enters', this.enters);
    api.events.on('leaves', this.leaves);
    api.events.on('playing', this.playing);
    api.events.on('stopped_playing', this.stopped_playing);
  }
  receive = (friends: TitleName[], msg: { playing: string[]; patrons: string[] }) => {
    this.users.clear();
    friends.forEach(this.insert);
    msg.playing
      .map(p => this.users.get(p))
      .filter(notNull)
      .forEach(u => (u.playing = true));
    msg.patrons
      .map(p => this.users.get(p))
      .filter(notNull)
      .forEach(u => (u.patron = true));
    this.repaint();
  };
  repaint = () => {
    if (this.loaded)
      requestAnimationFrame(() => {
        const ids = Array.from(this.users.keys()).sort();
        this.titleEl.innerHTML = i18n.site.nbFriendsOnline(
          ids.length,
          this.loaded ? `<strong>${ids.length}</strong>` : '-',
        );
        this.el.querySelector('.nobody')?.classList.toggle('none', !!ids[0]);
        this.el.querySelector('.list')!.innerHTML = ids
          .map(id => this.renderFriend(this.users.get(id)!))
          .join('');
      });
  };
  renderFriend = (friend: Friend) => {
    const icon = `<i class="line${friend.patron ? ' patron' : ''}"></i>`,
      titleTag = friend.title
        ? `<span class="utitle"${friend.title === 'BOT' ? ' data-bot' : ''}>${friend.title}</span>&nbsp;`
        : '',
      url = '/@/' + friend.name,
      tvButton = friend.playing
        ? `<a data-icon="${licon.AnalogTv}" class="tv ulpt" data-pt-pos="nw" href="${url}/tv" data-href="${url}"></a>`
        : '';
    return `<div><a class="user-link ulpt" data-pt-pos="nw" href="${url}">${icon}${titleTag}${friend.name}</a>${tvButton}</div>`;
  };

  enters = (titleName: TitleName, msg: { playing: boolean; patron: boolean }) => {
    const friend = this.insert(titleName);
    friend.playing = msg.playing;
    friend.patron = msg.patron;
    this.repaint();
  };
  leaves = (titleName: TitleName) => {
    this.users.delete(this.getId(titleName));
    this.repaint();
  };
  playing = (titleName: TitleName) => {
    this.insert(titleName).playing = true;
    this.repaint();
  };
  stopped_playing = (titleName: TitleName) => {
    this.insert(titleName).playing = false;
    this.repaint();
  };

  insert = (titleName: TitleName): Partial<Friend> => {
    const id = this.getId(titleName);
    if (!this.users.has(id)) this.users.set(id, this.toFriend(titleName));
    return this.users.get(id)!;
  };

  getId = (titleName: TitleName) => titleName.toLowerCase().replace(/^\w+\s/, '');

  toFriend = (titleName: TitleName): Friend => {
    const split = titleName.split(' ');
    return {
      id: split[split.length - 1].toLowerCase(),
      name: split[split.length - 1],
      title: split.length > 1 ? split[0] : undefined,
      playing: false,
      patron: false,
    };
  };
}
