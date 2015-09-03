
sealed trait Memory

case class GlobalMemory() extends Memory
case class LocalMemory() extends Memory
case class PrivateMemory() extends Memory
