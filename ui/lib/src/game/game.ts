import type { GameData, Player } from './interfaces';
import { finished, aborted, status } from './status';

export * from './interfaces';

export const playable = (data: GameData): boolean => data.game.status.id < status.aborted && !imported(data);

export const isPlayerPlaying = (data: GameData): boolean => playable(data) && !data.player.spectator;

export const isPlayerTurn = (data: GameData): boolean =>
  isPlayerPlaying(data) && data.game.player === data.player.color;

export const mandatory = (data: GameData): boolean => !!data.tournament || !!data.simul || !!data.swiss;

export const playedTurns = (data: GameData): number => data.game.turns - (data.game.startedAtTurn || 0);

export const bothPlayersHavePlayed = (data: GameData): boolean => playedTurns(data) > 1;

export const abortable = (data: GameData): boolean =>
  playable(data) && !bothPlayersHavePlayed(data) && !mandatory(data) && !data.game.rules?.includes('noAbort');

export const rematchable = (data: GameData): boolean => !data.game.rules?.includes('noRematch');

export const takebackable = (data: GameData): boolean =>
  playable(data) &&
  data.takebackable &&
  bothPlayersHavePlayed(data) &&
  !data.player.proposingTakeback &&
  !data.opponent.proposingTakeback;

export const drawable = (data: GameData): boolean =>
  playable(data) && data.game.turns >= 2 && !data.player.offeringDraw && !hasAi(data) && drawableSwiss(data);

export const drawableSwiss = (data: GameData): boolean => !data.swiss || playedTurns(data) >= 60;

export const resignable = (data: GameData): boolean => playable(data) && !abortable(data);

// can the current player go berserk?
export const berserkableBy = (data: GameData): boolean =>
  !!data.tournament && data.tournament.berserkable && isPlayerPlaying(data) && !bothPlayersHavePlayed(data);

export const moretimeable = (data: GameData): boolean =>
  isPlayerPlaying(data) &&
  data.moretimeable &&
  (!!data.clock ||
    (!!data.correspondence &&
      data.correspondence[data.opponent.color] < data.correspondence.increment - 3600));

export const imported = (data: GameData): boolean => data.game.source === 'import';

export const replayable = (data: GameData): boolean =>
  imported(data) || finished(data) || (aborted(data) && bothPlayersHavePlayed(data));

export function getPlayer(data: GameData, color: Color): Player;
export function getPlayer(data: GameData, color?: Color): Player | null {
  if (data.player.color === color) return data.player;
  if (data.opponent.color === color) return data.opponent;
  return null;
}

export const hasAi = (data: GameData): boolean => !!(data.player.ai || data.opponent.ai);

export const userAnalysable = (data: GameData): boolean =>
  finished(data) || (playable(data) && (!data.clock || !isPlayerPlaying(data)));

export const isCorrespondence = (data: GameData): boolean => data.game.speed === 'correspondence';

export const setOnGame = (data: GameData, color: Color, onGame: boolean): void => {
  const player = getPlayer(data, color);
  onGame = onGame || !!player.ai;
  player.onGame = onGame;
  if (onGame) setGone(data, color, false);
};

export const setGone = (data: GameData, color: Color, gone: number | boolean): void => {
  const player = getPlayer(data, color);
  player.isGone = !player.ai && gone;
  if (player.isGone === false && player.user) player.user.online = true;
};

export const nbMoves = (data: GameData, color: Color): number =>
  Math.floor((playedTurns(data) + (color === 'white' ? 1 : 0)) / 2);

export const isSwitchable = (data: GameData): boolean =>
  !hasAi(data) && (!!data.simul || isCorrespondence(data));

export const clockToSpeed = (initial: Seconds, increment: Seconds): Exclude<Speed, 'correspondence'> => {
  const total = initial + increment * 40;
  return total < 30
    ? 'ultraBullet'
    : total < 180
      ? 'bullet'
      : total < 480
        ? 'blitz'
        : total < 1500
          ? 'rapid'
          : 'classical';
};
