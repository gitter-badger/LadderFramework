package org.ladderframework

import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.ServerConnector
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.Request
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import java.net.URL
import scala.concurrent.Future
import org.eclipse.jetty.continuation.ContinuationSupport
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.util.parsing.input.StreamReader
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.collection.mutable.ArrayBuffer
import org.eclipse.jetty.server.handler.AsyncDelayHandler
import java.util.concurrent.atomic.AtomicInteger

class JettyServerSpec extends FunSpec with BeforeAndAfterAll with ScalaFutures{
	
	describe("JettyServer"){
		import scala.concurrent.ExecutionContext.Implicits.global
		it("should handle 50 concurrent requests"){
			val server = new JettyServerImpl
			server.start("127.0.0.1", 23457)
			Thread.sleep(100)
			try{
				val s = (0 until 50).par.map(i => {
					val found = readJava(new URL(s"http://localhost:23457?i=$i"))
					//val con = io.Source.fromURL(new URL(s"http://localhost:23457?i=$i"))
					//val found = con.getLines().mkString("\n") 
					assert(found === ("msg: " + i))
					//con.close()
					i
				}).sum
				assert(s !== 0)
			}finally{
				println("STOP")
				server.stop()
			}
		}
		def readJava(url: URL): String = {
       val con = url.openConnection();
	    con.setConnectTimeout(3000);
    	con.setReadTimeout(3000);
    	val in = new BufferedReader(new InputStreamReader(url.openStream))
    	val buffer = new ArrayBuffer[String]()
    	var inputLine = in.readLine
			while (inputLine != null) {
			    if (!inputLine.trim.equals("")) {
			        buffer += inputLine.trim
			    }
			    inputLine = in.readLine
			}
			in.close
			buffer.mkString("\n")
		}
	}
	
	class JettyServerImpl{
		val server = new Server();
		
		val executorService = Executors.newFixedThreadPool(10)
		implicit val executionContext = ExecutionContext.fromExecutorService(executorService)

		def sendMyTimeoutResponse(response: HttpServletResponse): Unit = {
			response.setStatus(408)
			val os = response.getOutputStream
			os.println("TIMEOUT")
			os.close()
		}
		
		def sendMyResultResponse(response: HttpServletResponse, msg: String): Unit = {
			response.setStatus(200)
			val os = response.getOutputStream
			os.print(msg)
			os.close()
		}
		
		def start(interface: String, port: Int): Unit = {
			// Set the connector
			val http = new ServerConnector(server);
			http.setHost(interface);
			http.setPort(port);
			http.setIdleTimeout(30000);
			http.setSoLingerTime(-1)
			server.addConnector(http);
	
			val atomicCounter = new AtomicInteger(0)
			// Set a handler
			val context = new ContextHandler();
			context.setContextPath( "/" );
			context.setResourceBase(".")
			val handler = new AsyncDelayHandler{
				override def handle(target: String,
							baseRequest: Request,
              httpServletRequest: HttpServletRequest ,
              httpServletResponse: HttpServletResponse ): Unit = {
						println("atomicCounter: " + atomicCounter.getAndIncrement)
						val i = httpServletRequest.getParameter("i")
						val continuation = ContinuationSupport.getContinuation(httpServletRequest)
						if (continuation.isExpired()){
							sendMyTimeoutResponse(httpServletResponse)
						}else{
							continuation.suspend(httpServletResponse); // response may be wrapped.
							Future{
								Thread.sleep(10)
								sendMyResultResponse(continuation.getServletResponse().asInstanceOf[HttpServletResponse],"msg: " + i);
								continuation.complete();
							}
						}
				}
			}
			context.setHandler(handler)
	
			server.setHandler(context)
			server.start();
			//server.join();
		}
	
		def stop(): Unit = {
			if (!(server.isStopped || server.isStopping)) {
				server.stop()
			}
		}
	}
	
}