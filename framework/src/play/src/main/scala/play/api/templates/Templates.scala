package play.api.templates

import play.api.mvc._
import play.templates._

/**
 * Appendable content using a StringBuilder.
 * @param elements StringBuilder to use
 * @tparam A self-type
 */
abstract class BufferedContent[A <: BufferedContent[A]](protected val elements: TraversableOnce[A], protected val text: String) extends Appendable[A] with Content with play.mvc.Content { this: A =>
  protected def buildString(builder: StringBuilder) {
    if (!elements.isEmpty) {
      elements.foreach { e =>
        e.buildString(builder)
      }
    }
    else {
      builder.append(text)
    }
  }

  /**
   * This should only ever be called at the top level element
   * to avoid unneeded memory allocation.
   */
  private lazy val builtBody = {
    val builder = new StringBuilder()
    buildString(builder)
    builder.toString
  }

  override def toString = builtBody

  def body = builtBody
}

/**
 * Content type used in default HTML templates.
 */
class Html private(elements: TraversableOnce[Html], text: String) extends BufferedContent[Html](elements, text) {
  def this(text: String) = this(Nil, text)
  def this(elements: TraversableOnce[Html]) = this(elements, "")

  /**
   * Content type of HTML (`text/html`).
   */
  def contentType: String = "text/html"

}

/**
 * Helper for HTML utility methods.
 */
object Html {

  /**
   * Creates an HTML fragment with initial content specified.
   */
  def apply(text: String): Html = {
    new Html(text)
  }
}

/**
 * Formatter for HTML content.
 */
object HtmlFormat extends Format[Html] {

  /**
   * Creates a raw (unescaped) HTML fragment.
   */
  def raw(text: String): Html = Html(text)

  /**
   * Creates a safe (escaped) HTML fragment.
   */
  def escape(text: String): Html = {
    // Using our own algorithm here because commons lang escaping wasn't designed for protecting against XSS, and there
    // don't seem to be any other good generic escaping tools out there.
    val sb = new StringBuilder(text.length)
    text.foreach {
      case '<' => sb.append("&lt;")
      case '>' => sb.append("&gt;")
      case '"' => sb.append("&quot;")
      case '\'' => sb.append("&#x27;")
      case '&' => sb.append("&amp;")
      case c => sb += c
    }
    new Html(sb.toString)
  }

  /**
   * Generate an empty HTML fragment
   */
  val empty: Html = new Html("")

  /**
   * Create an HTML Fragment that holds other fragments.
   */
  def fill(elements: TraversableOnce[Html]): Html = new Html(elements)

}

/**
 * Content type used in default text templates.
 *
 * @param text The plain text.
 */
class Txt private(elements: TraversableOnce[Txt], text: String) extends BufferedContent[Txt](elements, text) {
  def this(text: String) = this(Nil, text)
  def this(elements: TraversableOnce[Txt]) = this(elements, "")

  /**
   * Content type of text (`text/plain`).
   */
  def contentType = "text/plain"

}

/**
 * Helper for utilities Txt methods.
 */
object Txt {

  /**
   * Creates a text fragment with initial content specified.
   */
  def apply(text: String): Txt = {
    new Txt(text)
  }

}

/**
 * Formatter for text content.
 */
object TxtFormat extends Format[Txt] {

  /**
   * Create a text fragment.
   */
  def raw(text: String) = Txt(text)

  /**
   * No need for a safe (escaped) text fragment.
   */
  def escape(text: String) = Txt(text)

  /**
   * Generate an empty Txt fragment
   */
  val empty: Txt = new Txt("")

  /**
   * Create an Txt Fragment that holds other fragments.
   */
  def fill(elements: TraversableOnce[Txt]): Txt = new Txt(elements)

}

/**
 * Content type used in default XML templates.
 *
 * @param text the plain xml text
 */
class Xml private(elements: TraversableOnce[Xml], text: String) extends BufferedContent[Xml](elements, text) {
  def this(text: String) = this(Nil, text)
  def this(elements: TraversableOnce[Xml]) = this(elements, "")

  /**
   * Content type of XML (`text/xml`).
   */
  def contentType = "text/xml"

}

/**
 * Helper for XML utility methods.
 */
object Xml {

  /**
   * Creates an XML fragment with initial content specified.
   */
  def apply(text: String): Xml = {
    new Xml(text)
  }

}

/**
 * Formatter for XML content.
 */
object XmlFormat extends Format[Xml] {

  /**
   * Creates an XML fragment.
   */
  def raw(text: String) = Xml(text)

  /**
   * Creates an escaped XML fragment.
   */
  def escape(text: String) = Xml(org.apache.commons.lang3.StringEscapeUtils.escapeXml(text))

  /**
   * Generate an empty XML fragment
   */
  val empty: Xml = new Xml("")

  /**
   * Create an XML Fragment that holds other fragments.
   */
  def fill(elements: TraversableOnce[Xml]): Xml = new Xml(elements)

}

/** Defines a magic helper for Play templates. */
object PlayMagic {

  /**
   * Generates a set of valid HTML attributes.
   *
   * For example:
   * {{{
   * toHtmlArgs(Seq('id -> "item", 'style -> "color:red"))
   * }}}
   */
  def toHtmlArgs(args: Map[Symbol, Any]) = Html(args.map(a => a._1.name + "=\"" + HtmlFormat.escape(a._2.toString).body + "\"").mkString(" "))

}
