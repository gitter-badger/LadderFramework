package org.ladderframework.html

import scala.xml.NodeSeq
import org.ladderframework.Utils

package object form{
	
	implicit class FieldMappingRender[T](mapping: FieldMapping[T]){
		def text(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: TextFieldRenderer[T]) = {
			renderer(mapping, label, attrs.toMap, context)
		}
		
		def textarea(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: TextareaFieldRenderer[T]) = {
			renderer(mapping, label, attrs.toMap, context)
		}
	}
	
	implicit class NestedMappingRender1[A1 <: Mapping](nestedMapping: NestedMapping{type S = A1}){
		def render(transform: (A1) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender2[A1, A2](nestedMapping: NestedMapping{type S = Tuple2[A1, A2]}){
		def render(transform: (A1, A2) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender3[A1, A2, A3](nestedMapping: NestedMapping{type S = Tuple3[A1, A2, A3]}){
		def render(transform: (A1, A2, A3) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender4[A1, A2, A3, A4](nestedMapping: NestedMapping{type S = Tuple4[A1, A2, A3, A4]}){
		def render(transform: (A1, A2, A3, A4) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender5[A1, A2, A3, A4, A5](nestedMapping: NestedMapping{type S = Tuple5[A1, A2, A3, A4, A5]}){
		def render(transform: (A1, A2, A3, A4, A5) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
}

package form{
	trait TextFieldRenderer[T]{
		//symbols 'id, 'placeholder, 'help
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext): NodeSeq
	}
	
	object TextFieldRenderer{
		
		implicit object StringFieldRenderer extends TextFieldRenderer[String]{
			def apply(mapping: FieldMapping[String], label: String, attrs: Map[Symbol, String], context: FormContext): NodeSeq = {
				
				val id = attrs.getOrElse('id, Utils.uuid)
				
				val fieldErrors = context.errors.filter(_.key == mapping.key)
				
				<div class={"control-group" + {if(fieldErrors.isEmpty) "" else " error"}} >
					<label class="control-label" for={id} >{label}</label>
					<div class="controls">
						<input type="text" 
							name={mapping.key} 
							id={id} 
							placeholder={attrs.getOrElse('placeholder, "")} 
							value={context.data(mapping.key).getOrElse("")}/>
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{error.message}</span>)}
					</div>
				</div>
			}
		}
		
		implicit object IntFieldRenderer extends TextFieldRenderer[Int]{
			def apply(mapping: FieldMapping[Int], label: String, attrs: Map[Symbol, String], context: FormContext): NodeSeq = {
				
				val id = attrs.getOrElse('id, Utils.uuid)
				
				val fieldErrors = context.errors.filter(_.key == mapping.key)
				
				<div class={"control-group" + {if(fieldErrors.isEmpty) "" else " error"}} >
				<label class="control-label" for={id} >{label}</label>
				<div class="controls">
					<input type="number" 
						name={mapping.key} 
						id={id} 
						placeholder={attrs.getOrElse('placeholder, "")} 
						value={context.data(mapping.key).getOrElse("")}/>
					<span class="help-inline">{attrs.getOrElse('help, "")}</span>
					{fieldErrors.map(error => {						
							<span class="help-inline">{error.message}</span>
						})}
				</div>
			</div>
			}
		}
		
	}
		
	trait TextareaFieldRenderer[T]{
		//symbols 'id, 'placeholder, 'help
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext): NodeSeq
	}
	
	object TextareaFieldRenderer{
		
		implicit object StringFieldRenderer extends TextareaFieldRenderer[String]{
			def apply(mapping: FieldMapping[String], label: String, attrs: Map[Symbol, String], context: FormContext): NodeSeq = {
				
				val id = attrs.getOrElse('id, Utils.uuid)
				
				val fieldErrors = context.errors.filter(_.key == mapping.key)
				
				<div class={"control-group" + {if(fieldErrors.isEmpty) "" else " error"}} >
					<label class="control-label" for={id} >{label}</label>
					<div class="controls">
						<input type="text" 
							name={mapping.key} 
							id={id} 
							placeholder={attrs.getOrElse('placeholder, "")} 
							value={context.data(mapping.key).getOrElse("")}/>
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{error.message}</span>)}
					</div>
				</div>
			}
		}
	}

}

