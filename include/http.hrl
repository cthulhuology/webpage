-record(request, {
	stage = request_line, data = <<>>,
	method, path, protocol,
	headers = [],
	body = <<>>
}).

-record(response, {
	status = 200,
	reason = <<"OK">>,
	protocol = <<"HTTP/1.1">>,
	headers = [],
	body = <<>>
}).
