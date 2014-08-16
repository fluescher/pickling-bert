pickling-bert
=============


[![Build Status](https://api.travis-ci.org/fluescher/pickling-bert.png)](http://travis-ci.org/fluescher/pickling-bert)

Implements the [Erlang External Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html) for Scala (Work in progress).


Usage
-----

This project's artifacts are not yet deployed to a repository. To use it you have to build it manually (using `sbt package`) and add the resulting Jar to your classpath.

After that you can use it in your code:

```scala
import scala.pickling._
import bert.pickling._

val pickle = "hello world".pickle
val result = pickle.unpickle[String]

println(result)
```


Goals in the future are:

- All types of the Erlang Term Format read/writable
- Mapping of Scala Types to Erlang Term Format Types
- Usable as a scala-pickling format
