package shared

object PhoneNumber {

  def format(phoneNumber: String): String = phoneNumber.length match {
    case 7 => "([0-9]{3})([0-9]{4})".r.replaceFirstIn(phoneNumber, "$1-$2")
    case 10 => "([0-9]{3})([0-9]{3})([0-9]{4})".r.replaceFirstIn(phoneNumber, "($1) $2-$3")
    case 11 => "([0-9]{1})([0-9]{3})([0-9]{3})([0-9]{4})".r.replaceFirstIn(phoneNumber, "$1-$2-$3-$4")
    case _ => phoneNumber
  }

  def format(phoneNumber: String, extension: String): String = (phoneNumber, extension) match {
    case ("", _) => ""
    case (phone, "") => format(phone)
    case (phone, ext) => s"${format(phone)} ext. $ext"
  }
}
