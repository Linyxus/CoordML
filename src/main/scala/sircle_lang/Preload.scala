package sircle_lang

// preloaded source
object Preload {
  val preloadSource: String =
    """
      |def map = f: Any -> Any => xs: List => for x <- xs do f x
      |
      |def valuesOf = map: { } => for k <- keysOf map do map.k
      |
      |def itemsOf = map: { } => for k <- keysOf map do (k, map.k)
      |
      |def foldl = op: Any -> Any => z: Any => xs: List => {
      |  def ret = z;
      |  for x <- xs do {
      |    ret = op ret x
      |  };
      |  ret
      |}
      |
      |def foldl1 = op: Any -> Any => xs: List => foldl op (head xs) (tail xs)
      |
      |def prod = xs: List => foldl1 (x: List => y: List => x * y) xs
      |
      |def min = x => y => if x > y then y else x
      |
      |def max = x => y => if x > y then x else y
      |
      |def zipWith = op: Any -> Any -> Any => xs: List => ys: List => {
      |  def len = min (length xs) (length ys);
      |  for i <- range len do
      |    op (xs.i) (ys.i)
      |}
      |
      |def zip = zipWith $ x => y => (x, y)
      |
      |def restrict = keys: List => map: {} => buildMapping {
      |  for key <- keys do (key, map.key)
      |}
      |
      |def namedProd = map: {} => {
      |  def keys = keysOf map;
      |  def pairs = map toList $ prod $ valuesOf map;
      |  def namedPairs = map (zip keys) pairs;
      |  map buildMapping namedPairs
      |}
      |
      |def replicate = n: Int => x: Any => for i <- range n do x
      |
      |def cat = foldl1 (x: List => y: List => x + y)
      |
      |def compose = f: Any -> Any => g: Any -> Any => x: Any => f (g x)
      |
      |def flatMap = f: Any -> Any => compose cat $ map f
      |
      |def filter = f: Any -> Any => xs: List => for x <- xs, f x do x
      |
      |def println = x: Any => {
      |  print x; print "\n"
      |}
      |
      |def getFibonacci = n: Any => {
      |  if (n == 0) then []
      |  else if (n == 1) then [1]
      |  else {
      |    def ret = [1, 1];
      |    def x = 1;
      |    def y = 1;
      |    for i <- range (n - 2) do {
      |      def t = x + y;
      |      ret = ret + [t];
      |      x = y;
      |      y = t
      |    };
      |    ret
      |  }
      |}
      |
      |def mkSeq = xs: List => {
      |  def ret = head xs;
      |  for t <- tail xs do {
      |    ret = ret >> t
      |  };
      |  ret
      |}
      |
      |def mkPar = xs: List => {
      |  def ret = head xs;
      |  for t <- tail xs do {
      |    ret = ret || t
      |  };
      |  ret
      |}
      |
      |def fiboMain = n: Int => {
      |  println "fibonacci array";
      |  def arr = getFibonacci n;
      |  for i <- range n do {
      |    print "fibo(";
      |    print i;
      |    print ") = ";
      |    println (arr.i)
      |  }
      |}
      |
      |def showRange = n: Int => {
      |  println "range";
      |  for i <- range n, i > 5 do {
      |    print "range(";
      |    print i;
      |    print ") = ";
      |    println (range i)
      |  };
      |  "finished"
      |}
      |
      |def example0 = unused: Unit => {
      |  mkPar {
      |    for seed <- config."seeds" do {
      |      def c = restrict ["datasets", "base_model"] config;
      |      mkSeq $ map (x => mkTask "main.py" x x) (namedProd c)
      |    }
      |  }
      |}
      |
      |type Config =
      |  { "seeds": List
      |  , "datasets": List
      |  , "base_model": List
      |  }
      |
      |def config: Config =
      |  { "seeds" -> [1, 2, 3]
      |  , "datasets" -> ["avazu", "criteo"]
      |  , "base_model" -> ["DAutoInt", "DAutoInt+"]
      |  }
      |
      |def task: Task = mkTask "main.py" config config
      |""".stripMargin
}
