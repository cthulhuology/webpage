-record(request, {
	args,
	stage = request_line, 
	data = <<>>,
	method, path, protocol,
	headers = [],
	body = <<>>
}).

-record(response, {
	upgrade = false,
	args,
	status = 200,
	protocol = <<"HTTP/1.1">>,
	headers = [],
	body = <<>>
}).

-record(websocket, { 
	socket, 
	path, 
	headers, 
	module, 
	data, 
	state }).
