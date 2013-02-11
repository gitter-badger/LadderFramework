package org.ladderframework.html

import org.ladderframework.js.JsCmd
import org.ladderframework.Context
import scala.concurrent.Future

package object ajax {

	def onclick(callback: () => Future[JsCmd])(implicit context: Context): Map[String, String] = Map("onclick" -> context.addAjaxClickCallback(callback).toCmd)
	def onchange(callback: String => Future[JsCmd])(implicit context: Context): Map[String, String] = Map("onclick" -> context.addAjaxInputCallback(callback).toCmd)
}