
def WordInTexts(MapOfMaps: scala.collection.mutable.Map[String, Seq[(String, Double)]], word: String): Int =
var Text_contains = 0
for (k,v) <- MapOfMaps do
  if v.contains(word) then
    Text_contains = Text_contains+1
return Text_contains