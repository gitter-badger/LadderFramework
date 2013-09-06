package org.ladderframework.html.form

/**
 * Represents an object binding (ie. a binding for several fields).
 *
 * This is used for objects with one field. Other versions exist, e.g. `ObjectMapping2`, `ObjectMapping3`, etc.
 *
 * @tparam T the complex object type
 * @tparam A the first field type
 * @param apply a constructor function that creates a instance of `T` using field `A`
 * @param fa a mapping for field `A`
 * @param constraints constraints associated with this mapping
 */
case class ObjectMapping1[R, A1, M1 <: Mapping[M1]{type T = A1}](
		apply: Function1[A1, R], 
		unapply: Function1[R, Option[(A1)]], 
		f1: M1, 
		val key: String = "", 
		val constraints: Seq[Constraint[R]] = Nil
) extends NestedMapping[ObjectMapping1[R, A1, M1]] with ObjectMapping {

	type T = R
	type S = M1
	
	val field1: M1 = f1.withPrefix(key)
	
  def bind(data: Map[String, String]) = {
    merge(field1.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1]))
      }
    }
  }

  def unbind(value: R) = {
    unapply(value).map { fields =>
      val (v1) = fields
      val a1 = field1.unbind(v1)

      (a1._1) ->
        (a1._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String) = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*) = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = field1

}

case class ObjectMapping2[R, A1, A2, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}](
		apply: Function2[A1, A2, R], 
		unapply: Function1[R, Option[(A1, A2)]], 
		f1: M1, 
		f2: M2, 
		val key: String = "", 
		val constraints: Seq[Constraint[R]] = Nil
) extends NestedMapping[ObjectMapping2[R, A1, A2, M1, M2]] with ObjectMapping {

	type T = R
	type S = (M1, M2)
	
	val field1: M1 = f1.withPrefix(key)
	val field2: M2 = f2.withPrefix(key)
	
  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)

      (a1._1 ++ a2._1) ->
        (a1._2 ++ a2._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping2[R, A1, A2, M1, M2] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping2[R, A1, A2, M1, M2] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2)

}

case class ObjectMapping3[R, A1, A2, A3, 
	M1 <: Mapping[M1]{type T = A1}, 
	M2 <: Mapping[M2]{type T = A2}, 
	M3 <: Mapping[M3]{type T = A3}
](
		apply: Function3[A1, A2, A3, R], 
		unapply: Function1[R, Option[(A1, A2, A3)]], 
		f1: M1, 
		f2: M2, 
		f3: M3, 
		val key: String = "", 
		val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping3[R, A1, A2, A3, M1, M2, M3]] with ObjectMapping {

	val field1: M1 = f1.withPrefix(key)
	val field2: M2 = f2.withPrefix(key)
	val field3: M3 = f3.withPrefix(key)
	
	type T = R
	type S = (M1, M2, M3)
	
  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)

      (a1._1 ++ a2._1 ++ a3._1) ->
        (a1._2 ++ a2._2 ++ a3._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping3[R, A1, A2, A3, M1, M2, M3] = 
  		addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping3[R, A1, A2, A3, M1, M2, M3] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }
  
  val mappings = (field1, field2, field3)

}

case class ObjectMapping4[R, A1, A2, A3, A4, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}](
		apply: Function4[A1, A2, A3, A4, R], 
		unapply: Function1[R, Option[(A1, A2, A3, A4)]], 
		field1: M1, field2: M2, field3: M3, 
		field4: M4, val key: String = "", 
		val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping4[R, A1, A2, A3, A4, M1, M2, M3, M4]] with ObjectMapping {
	
	type T = R
	type S = (M1, M2, M3, M4)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping4[R, A1, A2, A3, A4, M1, M2, M3, M4] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping4[R, A1, A2, A3, A4, M1, M2, M3, M4] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4)

}

case class ObjectMapping5[R, A1, A2, A3, A4, A5, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}](apply: Function5[A1, A2, A3, A4, A5, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping5[R, A1, A2, A3, A4, A5, M1, M2, M3, M4, M5]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping5[R, A1, A2, A3, A4, A5, M1, M2, M3, M4, M5] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping5[R, A1, A2, A3, A4, A5, M1, M2, M3, M4, M5] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5)

}

case class ObjectMapping6[R, A1, A2, A3, A4, A5, A6, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}](apply: Function6[A1, A2, A3, A4, A5, A6, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping6[R, A1, A2, A3, A4, A5, A6, M1, M2, M3, M4, M5, M6]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping6[R, A1, A2, A3, A4, A5, A6, M1, M2, M3, M4, M5, M6] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping6[R, A1, A2, A3, A4, A5, A6, M1, M2, M3, M4, M5, M6] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6)

}

case class ObjectMapping7[R, A1, A2, A3, A4, A5, A6, A7, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}](apply: Function7[A1, A2, A3, A4, A5, A6, A7, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping7[R, A1, A2, A3, A4, A5, A6, A7, M1, M2, M3, M4, M5, M6, M7]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(
          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping7[R, A1, A2, A3, A4, A5, A6, A7, M1, M2, M3, M4, M5, M6, M7] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping7[R, A1, A2, A3, A4, A5, A6, A7, M1, M2, M3, M4, M5, M6, M7] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7)

}

case class ObjectMapping8[R, A1, A2, A3, A4, A5, A6, A7, A8, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}](apply: Function8[A1, A2, A3, A4, A5, A6, A7, A8, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping8[R, A1, A2, A3, A4, A5, A6, A7, A8, M1, M2, M3, M4, M5, M6, M7, M8]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping8[R, A1, A2, A3, A4, A5, A6, A7, A8, M1, M2, M3, M4, M5, M6, M7, M8] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping8[R, A1, A2, A3, A4, A5, A6, A7, A8, M1, M2, M3, M4, M5, M6, M7, M8] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8)

}

case class ObjectMapping9[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}](apply: Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping9[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1, M2, M3, M4, M5, M6, M7, M8, M9]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping9[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1, M2, M3, M4, M5, M6, M7, M8, M9] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping9[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1, M2, M3, M4, M5, M6, M7, M8, M9] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9)

}

case class ObjectMapping10[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}](apply: Function10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping10[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping10[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping10[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10)

}

case class ObjectMapping11[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}](apply: Function11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping11[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping11[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping11[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11)

}

case class ObjectMapping12[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}](apply: Function12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping12[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping12[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping12[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12)

}

case class ObjectMapping13[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}](apply: Function13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping13[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping13[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping13[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13)

}

case class ObjectMapping14[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}](apply: Function14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping14[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data), field14.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13],
          values(13).asInstanceOf[A14]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)
      val a14 = field14.unbind(v14)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping14[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping14[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14)

}

case class ObjectMapping15[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}](apply: Function15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping15[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data), field14.bind(data), field15.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13],
          values(13).asInstanceOf[A14],
          values(14).asInstanceOf[A15]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)
      val a14 = field14.unbind(v14)
      val a15 = field15.unbind(v15)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping15[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping15[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15)

}

case class ObjectMapping16[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}](apply: Function16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping16[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data), field14.bind(data), field15.bind(data), field16.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13],
          values(13).asInstanceOf[A14],
          values(14).asInstanceOf[A15],
          values(15).asInstanceOf[A16]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)
      val a14 = field14.unbind(v14)
      val a15 = field15.unbind(v15)
      val a16 = field16.unbind(v16)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping16[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping16[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16)

}

case class ObjectMapping17[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}, M17 <: Mapping[M17]{type T = A17}](apply: Function17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, field17: M17, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping17[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data), field14.bind(data), field15.bind(data), field16.bind(data), field17.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13],
          values(13).asInstanceOf[A14],
          values(14).asInstanceOf[A15],
          values(15).asInstanceOf[A16],
          values(16).asInstanceOf[A17]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)
      val a14 = field14.unbind(v14)
      val a15 = field15.unbind(v15)
      val a16 = field16.unbind(v16)
      val a17 = field17.unbind(v17)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1 ++ a17._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2 ++ a17._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping17[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping17[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17)

}

case class ObjectMapping18[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1 <: Mapping[M1]{type T = A1}, M2 <: Mapping[M2]{type T = A2}, M3 <: Mapping[M3]{type T = A3}, M4 <: Mapping[M4]{type T = A4}, M5 <: Mapping[M5]{type T = A5}, M6 <: Mapping[M6]{type T = A6}, M7 <: Mapping[M7]{type T = A7}, M8 <: Mapping[M8]{type T = A8}, M9 <: Mapping[M9]{type T = A9}, M10 <: Mapping[M10]{type T = A10}, M11 <: Mapping[M11]{type T = A11}, M12 <: Mapping[M12]{type T = A12}, M13 <: Mapping[M13]{type T = A13}, M14 <: Mapping[M14]{type T = A14}, M15 <: Mapping[M15]{type T = A15}, M16 <: Mapping[M16]{type T = A16}, M17 <: Mapping[M17]{type T = A17}, M18 <: Mapping[M18]{type T = A18}](apply: Function18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, field17: M17, field18: M18, val key: String = "", val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping[ObjectMapping18[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18]] with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18)

  def bind(data: Map[String, String]): Either[Seq[FormError], R] = {
    merge(field1.bind(data), field2.bind(data), field3.bind(data), field4.bind(data), field5.bind(data), field6.bind(data), field7.bind(data), field8.bind(data), field9.bind(data), field10.bind(data), field11.bind(data), field12.bind(data), field13.bind(data), field14.bind(data), field15.bind(data), field16.bind(data), field17.bind(data), field18.bind(data)) match {
      case Left(errors) => Left(errors)
      case Right(values) => {
        applyConstraints(apply(

          values(0).asInstanceOf[A1],
          values(1).asInstanceOf[A2],
          values(2).asInstanceOf[A3],
          values(3).asInstanceOf[A4],
          values(4).asInstanceOf[A5],
          values(5).asInstanceOf[A6],
          values(6).asInstanceOf[A7],
          values(7).asInstanceOf[A8],
          values(8).asInstanceOf[A9],
          values(9).asInstanceOf[A10],
          values(10).asInstanceOf[A11],
          values(11).asInstanceOf[A12],
          values(12).asInstanceOf[A13],
          values(13).asInstanceOf[A14],
          values(14).asInstanceOf[A15],
          values(15).asInstanceOf[A16],
          values(16).asInstanceOf[A17],
          values(17).asInstanceOf[A18]))
      }
    }
  }

  def unbind(value: R): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) = fields
      val a1 = field1.unbind(v1)
      val a2 = field2.unbind(v2)
      val a3 = field3.unbind(v3)
      val a4 = field4.unbind(v4)
      val a5 = field5.unbind(v5)
      val a6 = field6.unbind(v6)
      val a7 = field7.unbind(v7)
      val a8 = field8.unbind(v8)
      val a9 = field9.unbind(v9)
      val a10 = field10.unbind(v10)
      val a11 = field11.unbind(v11)
      val a12 = field12.unbind(v12)
      val a13 = field13.unbind(v13)
      val a14 = field14.unbind(v14)
      val a15 = field15.unbind(v15)
      val a16 = field16.unbind(v16)
      val a17 = field17.unbind(v17)
      val a18 = field18.unbind(v18)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1 ++ a17._1 ++ a18._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2 ++ a17._2 ++ a18._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  def withPrefix(prefix: String): ObjectMapping18[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18] = addPrefix(prefix).map(newKey => this.copy(key = newKey)).getOrElse(this)

  def verifying(addConstraints: Constraint[R]*): ObjectMapping18[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18] = {
    this.copy(constraints = constraints ++ addConstraints.toSeq)
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18)

}
