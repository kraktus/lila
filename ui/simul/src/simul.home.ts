import { wsConnect } from 'lib/socket';
import { pubsub } from 'lib/pubsub';

site.load.then(() => {
  wsConnect(`/socket/v5`, false, { params: { flag: 'simul' } });
  pubsub.on('socket.in.reload', async () => {
    const rsp = await fetch('/simul/reload');
    const html = await rsp.text();
    $('.simul-list__content').html(html);
    pubsub.emit('content-loaded');
  });
});
