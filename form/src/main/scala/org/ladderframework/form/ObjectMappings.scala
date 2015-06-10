package org.ladderframework.form

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
case class ObjectMapping1[R, A1, M1 <: Mapping{type T = A1}](
		apply: Function1[A1, R], 
		unapply: Function1[R, Option[(A1)]], 
		field1: M1, 
		key: String, 
		val constraints: Seq[Constraint[R]] = Nil
) extends NestedMapping with ObjectMapping {

	type T = R
	type S = M1
	
  override def bind(data: Map[String, String], prefix: String) = {
    field1.bind(data, prefix + optKey) match {
      case Left(errors) => Left(errors)
      case Right(v) =>
        applyConstraints(apply(v), prefix)
    }
  }

  override def unbind(value: R, prefix: String) = {
    unapply(value).map { fields =>
      val (v1) = fields
      val a1 = field1.unbind(v1, prefix + optKey)

      (a1._1) ->
        (a1._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = field1

}

case class ObjectMapping2[R, A1, A2, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}](
		apply: Function2[A1, A2, R], 
		unapply: Function1[R, Option[(A1, A2)]], 
		field1: M1, 
		field2: M2, 
		key: String, 
		val constraints: Seq[Constraint[R]] = Nil
) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2)
	
  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    (field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)) match {
      case (Left(le), Left(re)) => Left(le ++ re)
      case (_, Left(re)) => Left(re)
      case (Left(le), _) => Left(le)
      case (Right(lv), Right(rv)) => applyConstraints(apply(lv,rv), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { case (v1, v2) =>
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)

      (a1._1 ++ a2._1) ->
        (a1._2 ++ a2._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2)

}

case class ObjectMapping3[R, A1, A2, A3, 
	M1 <: Mapping{type T = A1}, 
	M2 <: Mapping{type T = A2}, 
	M3 <: Mapping{type T = A3}
](
		apply: Function3[A1, A2, A3, R], 
		unapply: Function1[R, Option[(A1, A2, A3)]], 
		field1: M1, 
		field2: M2, 
		field3: M3, 
		key: String, 
		val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3)
	
  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right(((v1, v2), v3)) =>
        applyConstraints(apply(v1, v2, v3), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { case (v1, v2, v3) =>
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1) ->
        (a1._2 ++ a2._2 ++ a3._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3)

}

case class ObjectMapping4[R, A1, A2, A3, A4, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}](
		apply: Function4[A1, A2, A3, A4, R], 
		unapply: Function1[R, Option[(A1, A2, A3, A4)]], 
		field1: M1, field2: M2, field3: M3, 
		field4: M4, key: String, 
		val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {
	
	type T = R
	type S = (M1, M2, M3, M4)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right((((v1, v2), v3), v4)) => 
        applyConstraints(apply(v1, v2, v3, v4), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { case (v1, v2, v3, v4) =>
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4)
  
}

case class ObjectMapping5[R, A1, A2, A3, A4, A5, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}](apply: Function5[A1, A2, A3, A4, A5, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right(((((v1, v2), v3), v4), v5)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }
  
  val mappings = (field1, field2, field3, field4, field5)

}

case class ObjectMapping6[R, A1, A2, A3, A4, A5, A6, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}](apply: Function6[A1, A2, A3, A4, A5, A6, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right((((((v1, v2), v3), v4), v5), v6)) => {
        applyConstraints(apply(v1, v2, v3, v4, v5, v6), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6)

}

case class ObjectMapping7[R, A1, A2, A3, A4, A5, A6, A7, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}](apply: Function7[A1, A2, A3, A4, A5, A6, A7, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)), field7.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right(((((((v1, v2), v3), v4), v5), v6), v7)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5, v6, v7), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7)

}

case class ObjectMapping8[R, A1, A2, A3, A4, A5, A6, A7, A8, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}](apply: Function8[A1, A2, A3, A4, A5, A6, A7, A8, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)), field7.bind(data, prefix + optKey)), field8.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right((((((((v1, v2), v3), v4), v5), v6), v7), v8)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5, v6, v7, v8), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8)

}

case class ObjectMapping9[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}](apply: Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)), field7.bind(data, prefix + optKey)), field8.bind(data, prefix + optKey)), field9.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right(((((((((v1, v2), v3), v4), v5), v6), v7), v8), v9)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5, v6, v7, v8, v9), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9)

}

case class ObjectMapping10[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}](apply: Function10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)), field7.bind(data, prefix + optKey)), field8.bind(data, prefix + optKey)), field9.bind(data, prefix + optKey)), field10.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right((((((((((v1, v2), v3), v4), v5), v6), v7), v8), v9), v10)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10)

}

case class ObjectMapping11[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}](apply: Function11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(mrg(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey)), field3.bind(data, prefix + optKey)), field4.bind(data, prefix + optKey)), field5.bind(data, prefix + optKey)), field6.bind(data, prefix + optKey)), field7.bind(data, prefix + optKey)), field8.bind(data, prefix + optKey)), field9.bind(data, prefix + optKey)), field10.bind(data, prefix + optKey)), field11.bind(data, prefix + optKey)) match {
      case Left(errors) => Left(errors)
      case Right(((((((((((v1, v2), v3), v4), v5), v6), v7), v8), v9), v10), v11)) => 
        applyConstraints(apply(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11), prefix)
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11)

}

case class ObjectMapping12[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}](apply: Function12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey)) match {
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
          values(11).asInstanceOf[A12]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12)

}

case class ObjectMapping13[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}](apply: Function13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey)) match {
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
          values(12).asInstanceOf[A13]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13)

}

case class ObjectMapping14[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}, M14 <: Mapping{type T = A14}](apply: Function14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey), field14.bind(data, prefix + optKey)) match {
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
          values(13).asInstanceOf[A14]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)
      val a14 = field14.unbind(v14, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14)

}

case class ObjectMapping15[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}, M14 <: Mapping{type T = A14}, M15 <: Mapping{type T = A15}](apply: Function15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey), field14.bind(data, prefix + optKey), field15.bind(data, prefix + optKey)) match {
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
          values(14).asInstanceOf[A15]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)
      val a14 = field14.unbind(v14, prefix + optKey)
      val a15 = field15.unbind(v15, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15)

}

case class ObjectMapping16[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}, M14 <: Mapping{type T = A14}, M15 <: Mapping{type T = A15}, M16 <: Mapping{type T = A16}](apply: Function16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey), field14.bind(data, prefix + optKey), field15.bind(data, prefix + optKey), field16.bind(data, prefix + optKey)) match {
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
          values(15).asInstanceOf[A16]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)
      val a14 = field14.unbind(v14, prefix + optKey)
      val a15 = field15.unbind(v15, prefix + optKey)
      val a16 = field16.unbind(v16, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16)

}

case class ObjectMapping17[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}, M14 <: Mapping{type T = A14}, M15 <: Mapping{type T = A15}, M16 <: Mapping{type T = A16}, M17 <: Mapping{type T = A17}](apply: Function17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, field17: M17, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey), field14.bind(data, prefix + optKey), field15.bind(data, prefix + optKey), field16.bind(data, prefix + optKey), field17.bind(data, prefix + optKey)) match {
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
          values(16).asInstanceOf[A17]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { case (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)  =>
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)
      val a14 = field14.unbind(v14, prefix + optKey)
      val a15 = field15.unbind(v15, prefix + optKey)
      val a16 = field16.unbind(v16, prefix + optKey)
      val a17 = field17.unbind(v17, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1 ++ a17._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2 ++ a17._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }


  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17)

}

case class ObjectMapping18[R, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, M1 <: Mapping{type T = A1}, M2 <: Mapping{type T = A2}, M3 <: Mapping{type T = A3}, M4 <: Mapping{type T = A4}, M5 <: Mapping{type T = A5}, M6 <: Mapping{type T = A6}, M7 <: Mapping{type T = A7}, M8 <: Mapping{type T = A8}, M9 <: Mapping{type T = A9}, M10 <: Mapping{type T = A10}, M11 <: Mapping{type T = A11}, M12 <: Mapping{type T = A12}, M13 <: Mapping{type T = A13}, M14 <: Mapping{type T = A14}, M15 <: Mapping{type T = A15}, M16 <: Mapping{type T = A16}, M17 <: Mapping{type T = A17}, M18 <: Mapping{type T = A18}](apply: Function18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, R], unapply: Function1[R, Option[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]], field1: M1, field2: M2, field3: M3, field4: M4, field5: M5, field6: M6, field7: M7, field8: M8, field9: M9, field10: M10, field11: M11, field12: M12, field13: M13, field14: M14, field15: M15, field16: M16, field17: M17, field18: M18, key: String, val constraints: Seq[Constraint[R]] = Nil) extends NestedMapping with ObjectMapping {

	type T = R
	type S = (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18)

  override def bind(data: Map[String, String], prefix: String): Either[Seq[FormError], R] = {
    merge(field1.bind(data, prefix + optKey), field2.bind(data, prefix + optKey), field3.bind(data, prefix + optKey), field4.bind(data, prefix + optKey), field5.bind(data, prefix + optKey), field6.bind(data, prefix + optKey), field7.bind(data, prefix + optKey), field8.bind(data, prefix + optKey), field9.bind(data, prefix + optKey), field10.bind(data, prefix + optKey), field11.bind(data, prefix + optKey), field12.bind(data, prefix + optKey), field13.bind(data, prefix + optKey), field14.bind(data, prefix + optKey), field15.bind(data, prefix + optKey), field16.bind(data, prefix + optKey), field17.bind(data, prefix + optKey), field18.bind(data, prefix + optKey)) match {
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
          values(17).asInstanceOf[A18]), prefix)
      }
    }
  }

  override def unbind(value: R, prefix: String): (Map[String, String], Seq[FormError]) = {
    unapply(value).map { fields =>
      val (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) = fields
      val a1 = field1.unbind(v1, prefix + optKey)
      val a2 = field2.unbind(v2, prefix + optKey)
      val a3 = field3.unbind(v3, prefix + optKey)
      val a4 = field4.unbind(v4, prefix + optKey)
      val a5 = field5.unbind(v5, prefix + optKey)
      val a6 = field6.unbind(v6, prefix + optKey)
      val a7 = field7.unbind(v7, prefix + optKey)
      val a8 = field8.unbind(v8, prefix + optKey)
      val a9 = field9.unbind(v9, prefix + optKey)
      val a10 = field10.unbind(v10, prefix + optKey)
      val a11 = field11.unbind(v11, prefix + optKey)
      val a12 = field12.unbind(v12, prefix + optKey)
      val a13 = field13.unbind(v13, prefix + optKey)
      val a14 = field14.unbind(v14, prefix + optKey)
      val a15 = field15.unbind(v15, prefix + optKey)
      val a16 = field16.unbind(v16, prefix + optKey)
      val a17 = field17.unbind(v17, prefix + optKey)
      val a18 = field18.unbind(v18, prefix + optKey)

      (a1._1 ++ a2._1 ++ a3._1 ++ a4._1 ++ a5._1 ++ a6._1 ++ a7._1 ++ a8._1 ++ a9._1 ++ a10._1 ++ a11._1 ++ a12._1 ++ a13._1 ++ a14._1 ++ a15._1 ++ a16._1 ++ a17._1 ++ a18._1) ->
        (a1._2 ++ a2._2 ++ a3._2 ++ a4._2 ++ a5._2 ++ a6._2 ++ a7._2 ++ a8._2 ++ a9._2 ++ a10._2 ++ a11._2 ++ a12._2 ++ a13._2 ++ a14._2 ++ a15._2 ++ a16._2 ++ a17._2 ++ a18._2)
    }.getOrElse(Map.empty -> Seq(FormError(key, "unbind.failed")))
  }

  val mappings = (field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18)

}
