webpage
=======

webpage is a SSL only web application server that supports https and wss. 

Getting Started
===============

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

	webpage


