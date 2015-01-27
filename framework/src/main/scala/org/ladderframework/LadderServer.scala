package org.ladderframework

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import org.ladderframework.logging.Loggable
import akka.util.Timeout
import akka.util.Timeout.durationToTimeout
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.ServerConnector
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.handler.ResourceHandler
import org.eclipse.jetty.server.handler.HandlerList

class LadderServer(boot: DefaultBoot) extends Loggable {

	val server = new Server();

	def start(interface: String, port: Int): Unit = {
		// Set the connector
		val http = new ServerConnector(server);
		http.setHost(interface);
		http.setPort(port);
		http.setIdleTimeout(30000);
		server.addConnector(http);

		// Set a handler
		val context = new ContextHandler();
		context.setContextPath( "/" );
		context.setResourceBase(".")
		context.setHandler( new LadderHandler(context, boot))

		server.setHandler(context)
		server.start();
		//server.join();
	}

	def stop(): Unit = {
		if (!(server.isStopped || server.isStopping)) {
			server.stop()
		}
		if (!boot.system.isTerminated) {
			boot.onShutdown()
			boot.system.shutdown()
		}
	}

}
