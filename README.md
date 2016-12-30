webpage
=======

webpage is a SSL only web application server that supports https and wss. 

Getting Started
---------------

Webpage is an Erlang OTP application, and as such requires that epmd be setup
and running on all of the nodes in your desired cluster. To start it manually
you can just type:

	epmd &

If you want to install epmd as a service in a systemd based system you can:

	sudo cp epmd.server /lib/systemd/system/
	sudo systemctl reenable epmd
	sudo systemctl start epmd

This will cause epmd to start whenever the multi-user.target is reached in
systemboot (which is usually what you want).

Before you can start webpage, you need to first initialize the mnesia database 
for your node:

	webpage test@localhost init

This will create a Mnesia.test@localhost directory containing the schema for 
your node.  You can then start the webpage application using:

	webpage test@localhost start

This will boot an empty application framework. If you have a preconfigured
application in a setup.conf file for webpage you can now load it:

	webpage test@localhost setup setup.conf

This will load the routes, files, and users into the database and make them
immediately avaiable.  

You can get a full list of command by typing

	webpage help


Routing
-------

The core concept behind webpage is that it maps HTTP requests to Erlang 
module functions.  A "route list" is a sequence of behaviors associated
with a path.  A path is a pattern that matches one or more URLs using a
wildcard syntax.  For example, the path:

	/chat/*

Will match all URLs which begin with /chat/, and so will match:

	/chat/
	/chat/foo
	/chat/foo/bar/narf

The rules are evaluated in order of most recent to least.  So you can
add additional rules to override the more generic paths.  For example,
we might want to specify a catchall rule, and then add user specific
rules afterwards:

	webpage route add "/app/*" '[ "my_app", "signup", [] ]'
	webpage route add "/app/dave" '[ [ "webpage_auth", "auth', [] ], [ "my_app", ["dave"] ] ]'

Here we would first add a cactchall that would bring the requestor to
a signup page, but for the user "dave", we might have a page that displays our
base application for his personalized path requiring authentication.

The first rule, invokes a method:

	my_app:signup(Request#request{ args = [] })

For each request to the /app/* paths, where as in the case of /app/dave it invokes a sequence
of methods:

	webpage_auth:auth(Request#request{ args = [] }),
	my_app:get(Response#response{ status = 401 })  

if authentication fails or 

	webpage_auth:auth(Request#request{ args = [] }),
	my_app:get(Request#request{ headers = [ <<"Email">>, <<"dave@dloh.org">> ], args = ["dave"]})

if authentication succeeds.

Here you can see the two types of route lists:

	MMA = module method arguments		[ Module, Args ]
	MFA = module function arguments		[ Module, Function, Args ]

In the MMA use case, the method is derived from the HTTP method, and can be one of 
get/1, post/1, put/1, delete/1, options/1, head/1.  The module should implement both
#request{} and #response{} versions of each of these methods. Each step in the 
route list will pass it's return value to the next operation in the chain.  In this,
way you can add layers of middleware to modify the value of a given operation.

In the MFA use case, the function is called regardless of the HTTP method.  This
is typical of middleware that provides authentication, compression, or sanity checking.
For example, you might want to add your own compression scheme to a response:

	webpage route add "/foo.zip"  '[ [ "webpage_file", [] ], [ "my_zip", "zip", [] ] ]'

Which will call my_zip:zip(#response{ args = [] }) on the output of webpage_file.  So a
request to

	GET /foo.zip HTTP/1.1

Will result in the file mapped by webpage_file to that path to return with zip compression
enabled.  

It is important to note that each path will map to only one route list, and as such you 
should construct the full chain for any route you create.  

Each route is listed at a special /routes/ path, and has a corresponding JSON object you
can inspect and modify via the REST interface.  By default no access to this path is granted,
and no routes explicitly map to it.


Authentication
--------------

webpage provides basic auth for users through the webpage_auth module.  You can replace this
module with your own authentication system, or can add any number of authentication schemes
via route lists, and it is not mandatory.

To enforce authentication on a path, you should start the route list with

	[ "webpage_auth", "auth", [] ]

This will compare the basic auth token of the request against the granted paths associated
with that token.  To grant access to every path one could:

	webpage user grant admin "/*"

Which will allow the user "admin" to access all paths.  To restrict a user to only some paths
one might do something like:

	webpage user grant bob "/forums/*"

Allowing the user bob access only to "/forums/" paths that require authentication.  In this
way, you can grant access either by:

	1.) not including authentication for a path, making it public
	2.) requiring authentication for a path, and then granted access to individuals

Access can be fine grained limiting a user to only specific paths:

	webpage user grant bob "/users/bob"

This allows for tailoring access to just those behaviors you want to expose to a user.  

You can test authentication for a user if you have their password:

	webpage user auth "/users/bob" bob testpassword

The webpage database does not store a plain text version of the user's password, but a
salted hmac sha256 version of the token.  You should configure your site specific salt 
in your app environment.


Files
-----

Unlike most webservers, webpage does not provide any access to the host file system by default.
Files can be exposed through route lists using the webpage_file module.  This module requires
that each file to be exposed to a path be registered with the database before being accessible.
In this way, webpage maps a path to a local file system file, and provides read-only access to
it's contents.
	
	webpage file add "/images/foo.png" "/var/www/images/foo.png" "image/png"

This will restrict the path "/images/foo.png" to the content of the file at "/var/www/images/foo.png"
with a mimetype of "image/png".  The file mapping requires an explicit mimetype be supplied at
mapping time so that server knows how the user intends to use the file's contents.  This  directive
will also add the assocaited path mapping to the webpage_file module.  You can override the 
default route list mapping by running the command:

	webpage route add "/image/foo.png" '[ [ "webpage_auth","auth",[] ], [ "webpage_file", [] ] ]'

which would then restrict access to only authenticated users.  The default route list added makes
the file public.  Both the file registration and the route list must exist for the contents of the
file to be accessible, and the path must match for both!

The full list of file mapped paths can be queries with:

	webpage file list

It is important to note that a file can be mapped but not routed.  
