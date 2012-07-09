package bootstrap

import org.ladderframework.HttpResponse
import org.ladderframework.HttpRequest
import org.ladderframework.DefaultBoot

object LadderBoot extends DefaultBoot{

	var site: PartialFunction[HttpRequest, HttpResponse] = {
		case _ => throw new IllegalStateException("Create a bootstrap.LadderBoot that extends org.ladderframework.DefaultBoot")
		case HttpRequest(GET, path, _, _, _) => 
			val app = service.get(path)
			HtmlPage(app)
	}
	
}