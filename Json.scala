/**
 * Created by gaoyunxiang on 8/22/15.
 */

import java.lang.reflect.{Field, Modifier}
import scala.collection.mutable

object Json {

    class Value(inputValue: Any) {

        def asAny: Any = value

        def isNull: Boolean = value == null

        def asShort: Short = value.toString.toShort
        def isShort: Boolean = if (isLong) asLong.toShort == asLong else false

        def asInt: Int = value.toString.toInt
        def isInt: Boolean = if (isLong) asLong.toInt == asLong else false

        def asLong: Long = value.asInstanceOf[Long]
        def isLong: Boolean = value.isInstanceOf[Long]

        def asFloat: Float = value.toString.toFloat

        def asDouble: Double = value.asInstanceOf[Double]
        def isDouble: Boolean = value.isInstanceOf[Double]

        def asNumber: Number = value.asInstanceOf[Number]
        def isNumber: Boolean = value.isInstanceOf[Number]

        def asBoolean: Boolean = value.asInstanceOf[Boolean]
        def isBoolean: Boolean = value.isInstanceOf[Boolean]

        def asString: String = value.asInstanceOf[String]
        def isString: Boolean = value.isInstanceOf[String]

        def asArray: Array[Value] = value.asInstanceOf[Array[Value]]
        def isArray: Boolean = value.isInstanceOf[Array[_]]

        def asMap: Map[String, Value] = value.asInstanceOf[Map[String, Value]]
        def isMap: Boolean = value.isInstanceOf[Map[_, _]]

        def apply(i: Int): Value =
            if (isArray)
                asArray(i)
            else
                throw new Exception("field is not Array")

        def apply(key: String): Value =
            if (isMap)
                asMap(key)
            else
                throw new Exception("field is not Map")

        def write(): String = {
            val buffer = new mutable.StringBuilder()
            recWrite(buffer)
            buffer.toString()
        }

        override def toString: String = String.valueOf(inputValue)

        private val value: Any = inputValue match {
            case null => null
            case v: Int => v.toLong
            case v: Long => v
            case v: Double => v
            case v: Boolean => v
            case v: String => v
            case v: Value => v.value
            case v: Map[_, _] =>
                v.map(one =>
                    (one._1.toString, Value(one._2)))
            case v: Iterable[_] => v.map(Value(_)).toArray
            case v: mutable.Iterable[_] => v.map(Value(_)).toArray
            case v: Array[_] => v.map(Value(_))

            case _ => throw new Exception("unknow type")
        }

        private def recWrite(buffer: mutable.StringBuilder): Unit = {
            value match {
                case null => buffer.append("null")
                case v: Long => buffer.append(v)
                case v: Boolean => buffer.append(v)
                case v: Double => buffer.append(v)
                case v: String =>
                    buffer.append('"')
                    v.foreach {
                        each => {
                            if (each == '\\' || each == '"') {
                                buffer.append('\\')
                            } else if (each == '\b') {
                                buffer.append("\\b")
                            } else if (each == '\f') {
                                buffer.append("\\f")
                            } else if (each == '\n') {
                                buffer.append("\\n")
                            } else if (each == '\r') {
                                buffer.append("\\r")
                            } else if (each == '\t') {
                                buffer.append("\\t")
                            } else {
                                buffer.append(each)
                            }
                        }
                    }
                    buffer.append('"')
                case v: Array[_] =>
                    buffer.append('[')
                    for (i <- v.indices) {
                        if (i != 0) {
                            buffer.append(',')
                        }
                        v(i).asInstanceOf[Value].recWrite(buffer)
                    }
                    buffer.append(']')
                case v: Map[_, _] =>
                    buffer.append('{')
                    var first = true
                    v.foreach {
                        one =>
                            if (!first) {
                                buffer.append(',')
                            }
                            first = false
                            buffer.append('"')
                            buffer.append(one._1)
                            buffer.append('"')
                            buffer.append(':')
                            one._2.asInstanceOf[Value].recWrite(buffer)
                    }
                    buffer.append('}')
                case _ => throw new Exception("unknow data type")
            }
        }
    }
    object Value {
        def apply(inputValue: Any): Value = {
            new Value(inputValue)
        }
    }

    def parse(p: String): Value = {
        val sta = mutable.ArrayBuffer[(Char, Any)]()
        var i = 0
        while (i < p.length) {
            if (p(i).isWhitespace || p(i) == '-' || p(i) == ':' || p(i) == ',') {
            } else if (p(i) == '[') {
                sta.append(('[', null))
            } else if (p(i) == '{') {
                sta.append(('{', null))
            } else if (p(i) == ']') {
                val vec = mutable.Stack[Value]()
                while (sta.nonEmpty && sta.last._1 != '[') {
                    vec.push(Value(sta.last._2))
                    sta.trimEnd(1)
                }
                if (sta.isEmpty || sta.last._1 != '[') {
                    throw JsonParseException("parse error, [] not match")
                }
                sta.trimEnd(1)
                sta.append(('a', vec))
            } else if (p(i) == '}') {
                val now = mutable.ListBuffer[(String, Any)]()

                while (sta.length >= 2 && sta.last._1 != '{') {
                    val new_value: Value = Value(sta.last._2)
                    sta.trimEnd(1)
                    val new_key = sta.last._2.asInstanceOf[String]
                    sta.trimEnd(1)
                    now addOne(new_key, new_value)
                }
                if (sta.isEmpty || sta.last._1 != '{') {
                    throw JsonParseException("parse error, {} not match")
                }
                sta.trimEnd(1)
                sta.append(('o', now.reverse.toMap))
            } else if (p(i) == '"') {
                var j = i + 1
                val S = new mutable.StringBuilder()
                while (j < p.length && p(j) != '"') {
                    if (p(j) == '\\') {
                        if (p(j + 1) == 'b') {
                            S.append('\b')
                        } else if (p(j + 1) == 'f') {
                            S.append('\f')
                        } else if (p(j + 1) == 'n') {
                            S.append('\n')
                        } else if (p(j + 1) == 'r') {
                            S.append('\r')
                        } else if (p(j + 1) == 't') {
                            S.append('\t')
                        } else {
                            S.append(p(j + 1))
                        }
                        j += 2
                    } else {
                        S.append(p(j))
                        j += 1
                    }
                }
                sta.append(('v', S.toString()))
                i = j
            } else if (p(i).isDigit) {
                val is_double = {
                    var j = i + 1
                    while (j < p.length && p(j).isDigit) {
                        j += 1
                    }
                    j < p.length && (p(j) == '.' || p(j) == 'e' || p(j) == 'E')
                }
                val minus_flag = if (i > 0 && p(i - 1) == '-') {
                    -1
                } else {
                    1
                }
                var j = i
                while (j < p.length && p(j) != ',' && p(j) != ']' && p(j) != '}') {
                    j += 1
                }
                if (is_double) {
                    val v = p.substring(i, j).trim.toDouble * minus_flag
                    sta.append(('d', v))
                } else {
                    val v = p.substring(i, j).trim.toLong * minus_flag
                    sta.append(('i', v))
                }
                i = j - 1
            } else if (p.substring(i, i + 4).trim == "null") {
                sta.append(('n', null))
                i += 3
            } else if (p.substring(i, i + 4).trim == "true") {
                sta.append(('b', true))
                i += 3
            } else if (p.substring(i, i + 5).trim == "false") {
                sta.append(('b', false))
                i += 4
            } else {
                throw JsonParseException(p, i)
            }
            i += 1
        }
        if (sta.length != 1) {
            throw JsonParseException("parse error, type=final")
        }
        Value(sta.head._2)
    }

    class JsonParseException(private val msg: String) extends Exception() {
        override def getMessage: String = msg
    }
    object JsonParseException {
        def apply(msg: String): JsonParseException = new JsonParseException(msg)
        def apply(json: String, position: Int): JsonParseException = new JsonParseException(buildMessage(json, position))

        private def buildMessage(json: String, position: Int): String = {
            val lines = json.split(System.lineSeparator)
            var errLineNo: Int = 0
            var errLineText: String = ""
            var linePosition: Int = 0

            var length = 0
            while (errLineNo < lines.size && length < position) {
                length += lines(errLineNo).length
                errLineNo += 1
            }
            errLineText = lines(errLineNo - 1)
            // due to split, lineSeparator has been removed
            linePosition = position - lines.take(errLineNo - 1).foldLeft(0) { (acc, line) => acc + (line + System.lineSeparator).length }

            s"${System.lineSeparator}\tat position [$position]${System.lineSeparator}" +
                s"\tat line position [$linePosition]>>>${lines(errLineNo - 1)(linePosition)}${System.lineSeparator}" +
                s"\tat error line [$errLineNo]>>>${lines(errLineNo - 1)}${System.lineSeparator}" +
                s"\tat json>>>${json.substring(position)}"
        }
    }

    def toJson(any: Any): String = {
        any match {
            case null => "null"
            case v: String => "\"" + v + "\""
            case v: Number => v.toString
            case v: Char => v.toString
            case v: Boolean => v.toString
            case v: Array[_] => v.map(toJson).mkString("[", ",", "]")
            case v: Iterable[_] => v.map(toJson).mkString("[", ",", "]")
            case v: mutable.Iterable[_] => v.map(toJson).mkString("[", ",", "]")
            case v: Map[_, _] => v.map { it => "\"" + it._1 + "\":" + toJson(it._2) }.mkString("{", ",", "}")
            case _ => objectToJson(any)
        }
    }

    private def objectToJson(obj: Any): String = {
        if (obj == null) {
            toJson(null)
        } else {
            val map = mutable.Map[String, Any]()
            val fields = mutable.ListBuffer[Field]()
            var currentClass = obj.getClass
            while (currentClass != null) {
                fields ++= currentClass.getDeclaredFields
                currentClass = currentClass.getSuperclass
            }
            fields.foreach { field =>
                if (isReadable(field.getModifiers)) {
                    if (field.canAccess(obj)) {
                        field.setAccessible(true)
                    }
                    map += (field.getName -> field.get(obj))
                } else {
                    obj.getClass.getDeclaredMethods.find { method =>
                        isReadable(method.getModifiers) &&
                            (method.getName == field.getName || method.getName == field.getName.substring(1))
                    } match {
                        case Some(getter) => map += (getter.getName -> getter.invoke(obj))
                        case _ => None
                    }
                }
            }
            map.map(kv => "\"" + kv._1 + "\":" + toJson(kv._2)).mkString("{", ",", "}")
        }
    }

    private def isReadable(modifiers: Int): Boolean = {
        !Modifier.isPrivate(modifiers) && !Modifier.isProtected(modifiers)
    }

}
