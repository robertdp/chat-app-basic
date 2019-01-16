exports._upgradeServer = function(server) {
  var io = require('socket.io');
  return io(server);
};

exports._on = function(subject, eventName, handler) {
  subject.on(eventName, handler);
};

exports._emit = function(socket, eventName, msg) {
  socket.emit(eventName, msg);
};

exports._broadcast = function(socket, eventName, msg) {
  socket.broadcast.emit(eventName, msg);
};