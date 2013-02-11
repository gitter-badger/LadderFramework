package bootstrap

import org.ladderframework.HttpResponse
import org.ladderframework.HttpRequest
import org.ladderframework.DefaultBoot
import scala.concurrent.Future

object LadderBoot extends DefaultBoot{

	var site: PartialFunction[HttpRequest, Future[HttpResponse]] = {
		case _ => throw new IllegalStateException("Create a bootstrap.LadderBoot that extends org.ladderframework.DefaultBoot")
	}
	
}