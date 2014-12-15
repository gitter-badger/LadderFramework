package org.ladderframework

import scala.concurrent.Future
import java.net.URL
import org.ladderframework.Method._

object LadderTestApp extends App{

	val server = new LadderServer(new DefaultBoot{
		def site = {
			case GET(Path("form" :: Nil)) => Future.successful(HtmlResponse(<form action="/formpost" method="post" enctype="multipart/form-data">
						<p><input type="text" name="text" value="text default"/></p>
						<p><input type="file" name="file1"/></p>
						<p><input type="file" name="file2"/></p>
						<p><button type="submit">Submit</button></p>
					</form>.toString))
			case GET(Path(Nil)) => Future.successful(XmlResponse(<div>Jeg er glad!!<a href="/form">form</a></div>))
		}
	})
	
	server.start("127.0.0.1", 23023)
	
	Thread.sleep(250000)
	server.stop()
	
}