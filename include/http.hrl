-compile({no_auto_import,[get/1]}).

-record(request, {
	socket, module, function, args,
	stage = request_line, 
	data = <<>>,
	method, path, protocol,
	headers = [],
	body = <<>>
}).

-record(response, {
	socket, module, function, args, upgrade = false,
	status = 200,
	protocol = <<"HTTP/1.1">>,
	headers = [],
	body = <<>>
}).

-record(websocket, { 
	socket,
	request,
	module, 
	function,
	args,
	data, 
	state }).
