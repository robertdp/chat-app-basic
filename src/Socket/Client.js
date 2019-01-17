exports.createSocket = function(url) {
  var io = require('socket.io-client');
  return function() {
    return io(url);
  };
};

exports._on = function(socket, event, handler) {
  socket.on(event, handler);
};

exports._emit = function(socket, event, msg) {
  socket.emit(event, msg);
};