package org.ladderframework.html

import scala.xml.NodeSeq
import org.ladderframework.Utils
import scala.xml.Text
import bootstrap.LadderBoot
import java.util.ResourceBundle
import java.text.MessageFormat

package object form{
	
	implicit class FieldMappingRender[T](mapping: FieldMapping[T]){
		def text(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: TextFieldRenderer[T], errorTransformer: ErrorMessageTransformer) = {
			renderer(mapping, label, attrs.toMap, context, errorTransformer)
		}
		
		def textarea(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: TextareaFieldRenderer[T], errorTransformer: ErrorMessageTransformer) = {
			renderer(mapping, label, attrs.toMap, context, errorTransformer)
		}
		
		def password(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: PasswordFieldRenderer[T], errorTransformer: ErrorMessageTransformer) = {
			renderer(mapping, label, attrs.toMap, context, errorTransformer)
		}
		
		def checkbox(label: String, attrs: Tuple2[Symbol, String]*)(implicit context: FormContext, renderer: CheckboxFieldRenderer[T], errorTransformer: ErrorMessageTransformer) = {
			renderer(mapping, label, attrs.toMap, context, errorTransformer)
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

	implicit class NestedMappingRender6[A1, A2, A3, A4, A5, A6](nestedMapping: NestedMapping{type S = Tuple6[A1, A2, A3, A4, A5, A6]}){
		def render(transform: (A1, A2, A3, A4, A5, A6) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender7[A1, A2, A3, A4, A5, A6, A7](nestedMapping: NestedMapping{type S = Tuple7[A1, A2, A3, A4, A5, A6, A7]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}

	implicit class NestedMappingRender8[A1, A2, A3, A4, A5, A6, A7, A8](nestedMapping: NestedMapping{type S = Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender9[A1, A2, A3, A4, A5, A6, A7, A8, A9](nestedMapping: NestedMapping{type S = Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}

	implicit class NestedMappingRender10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](nestedMapping: NestedMapping{type S = Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](nestedMapping: NestedMapping{type S = Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
	
	implicit class NestedMappingRender12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](nestedMapping: NestedMapping{type S = Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}

	implicit class NestedMappingRender13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](nestedMapping: NestedMapping{type S = Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]}){
		def render(transform: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => NodeSeq => NodeSeq): NodeSeq => NodeSeq = transform.tupled(nestedMapping.mappings)
	}
}

package form{
	
	trait ErrorMessageTransformer{
		def apply(error: FormError): NodeSeq
	}
	object ErrorMessageTransformer{
		implicit object MessageFileErrorMessageTransformer extends ErrorMessageTransformer{
			
			lazy val bundle = ResourceBundle.getBundle("ValidationMessages")
			
			def apply(error: FormError): NodeSeq = {
				if(bundle.containsKey(error.message)) {
					val msg = bundle.getString(error.message)
					Text(MessageFormat.format(msg, error.args.map(_.asInstanceOf[AnyRef]):_*))
				}else {
					Text(s"unable to find key '${error.message}' in ValidationMessage.properties")
				}
				
			}			
		}
	}
	
	trait TextFieldRenderer[T]{
		//symbols 'id, 'placeholder, 'help
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq
	}
	
	object TextFieldRenderer{
		
		implicit object StringFieldRenderer extends TextFieldRenderer[String]{
			def apply(mapping: FieldMapping[String], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq = {
				
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
						{
							if(mapping.constraints.map(_.name).exists(_ == Constraints.nonEmpty.name)) <span>*</span> else NodeSeq.Empty
						}
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{errorTransformer(error)}</span>)}
					</div>
				</div>
			}
		}
		
		implicit object IntFieldRenderer extends TextFieldRenderer[Int]{
			def apply(mapping: FieldMapping[Int], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq = {
				
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
						{
							if(mapping.constraints.map(_.name).exists(_ == Constraints.nonEmpty.name)) <span>*</span> else NodeSeq.Empty
						}
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => {						
							<span class="help-inline">{errorTransformer(error)}</span>
						})}
					</div>
				</div>
			}
		}
		
	}
		
	trait TextareaFieldRenderer[T]{
		//symbols 'id, 'placeholder, 'help
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq
	}
	
	object TextareaFieldRenderer{
		
		implicit object StringFieldRenderer extends TextareaFieldRenderer[String]{
			def apply(mapping: FieldMapping[String], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq = {
				
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
						{
							if(mapping.constraints.map(_.name).exists(_ == Constraints.nonEmpty.name)) <span>*</span> else NodeSeq.Empty
						}
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{errorTransformer(error)}</span>)}
					</div>
				</div>
			}
		}
	}
	
	trait PasswordFieldRenderer[T]{
		//symbols 'id, 'placeholder, 'help
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq
	}
	
	object PasswordFieldRenderer{
		
		implicit object StringFieldRenderer extends PasswordFieldRenderer[String]{
			def apply(mapping: FieldMapping[String], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq = {
				
				val id = attrs.getOrElse('id, Utils.uuid)
				
				val fieldErrors = context.errors.filter(_.key == mapping.key)
				
				<div class={"control-group" + {if(fieldErrors.isEmpty) "" else " error"}} >
					<label class="control-label" for={id} >{label}</label>
					<div class="controls">
						<input type="password" 
							name={mapping.key} 
							id={id} 
							placeholder={attrs.getOrElse('placeholder, "")} 
							value={context.data(mapping.key).getOrElse("")}/>
						{
							if(mapping.constraints.map(_.name).exists(_ == Constraints.nonEmpty.name)) <span>*</span> else NodeSeq.Empty
						}
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{errorTransformer(error)}</span>)}
					</div>
				</div>
			}
		}
	}
	
	trait CheckboxFieldRenderer[T]{
		//symbols 'id
		def apply(mapping: FieldMapping[T], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq
	}
	
	object CheckboxFieldRenderer{
		
		implicit object StringFieldRenderer extends PasswordFieldRenderer[Boolean]{
			def apply(mapping: FieldMapping[Boolean], label: String, attrs: Map[Symbol, String], context: FormContext, errorTransformer: ErrorMessageTransformer): NodeSeq = {
				
				val id = attrs.getOrElse('id, Utils.uuid)
				
				val fieldErrors = context.errors.filter(_.key == mapping.key)
				
				<div class={"control-group" + {if(fieldErrors.isEmpty) "" else " error"}} >
					<div class="controls">
						<label class="checkbox">
							<input type="checkbox"
								name={mapping.key} 
								id={id}
								checked={if(context.data(mapping.key).exists(_ == true.toString)) Some(Seq(Text("checked"))) else None}
								value="true"
							/> {label}
						</label>
						<input type="hidden" 
							name={mapping.key} 
							value="false"/>
						<span class="help-inline">{attrs.getOrElse('help, "")}</span>
						{fieldErrors.map(error => <span class="help-inline">{errorTransformer(error)}</span>)}
					</div>
				</div>
			}
		}
	}
	
	

}

