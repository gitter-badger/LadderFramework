package org.ladderframework

import scala.concurrent.Future
import java.net.URL
import org.ladderframework.Method._

object LadderTestApp extends App{

	val server = new LadderServer(new DefaultBoot{
		def site = {
			case GET(Path("form" :: Nil)) => Future.successful(HtmlResponse(<form action="/formpost" method="post" enctype="multipart/form-data">
						<p><input type="t	ext" name="text" value="text default"/></p>
						<p><input type="file" name="file1"/></p>
						<p><input type="file" name="file2"/></p>
						<p><button type="submit">Submit</button></p>
					</form>.toString))
			case req@POST(Path("formpost" :: Nil)) => 
				for {
					p <- req.parts
				} yield {
					p.foreach(println)
					XmlResponse(<div>Jeg er glad!!<a href="/form">form</a></div>)
				}
			case GET(Path(Nil)) => Future.successful(HtmlResponse(<div>Jeg er glad!!<a href="/form">form</a></div>.mkString))
		}
	})
	
	server.start("127.0.0.1", 23023)
	
	val mainThread = Thread.currentThread();
		Runtime.getRuntime().addShutdownHook(new Thread() {
			override def run(): Unit = {
				println("Shutting down")
				server.stop()
        mainThread.join();
			}
		});
	Thread.sleep(200)
	io.StdIn.readLine()
	server.stop()
	
}