# Proof of concept code for R-Tetrad interface

The file `tetrad_in_r_tutorial.R` gives a worked example: in it, I bring some data into R as a dataframe, turn that R dataframe into a Tetrad DataSet, instantiate GES with that dataset, search for a pattern with GES, and bring the pattern back into R as a graphNEL object.

## Choices to be made

There are two things that must be decided on before this can be turned into a proper interface to Tetrad:

1. The graphNEL object I created is unable to distinguish between bidirected and undirected edges. We'll want to use a different representation. I only picked this format because the `pcalg` package uses it for the output of PC, so it allowed me to compare Tetrad results with `pcalg` results. I'll look at the format `pcalg` uses for the output of FCI, which must be more expressive. I hope we can get away with using an existing representation rather than building our own graph library for R.

2. We probably want to do a combination of the following: (a) wrap popular Tetrad functions in R and put them into an R package for ease of use, and (b) explain how experienced users can write their own wrappers if they would like to access more Tetrad functions. We ought to decide what to include in the R package, and what to change in the Tetrad package so that the R package is easier to maintain.

## Things I have learned about rJava

rJava has frustrated me a lot. Here are the important things I've learned:

1. I started with serious config issues - first rJava would install but would use Java 1.6 instead of 1.8, then it wouldn't install at all. I finally got it working though. If you have the same issues, my solution is documented in this Stack Overflow answer: http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358

2. rJava has some super helpful functions, `.jmethods` and `jconstructors`, for figuring out what methods and constructors are available for a given class. I don't yet know if there's a way to explore the set of classes.

3. There are two different APIs in rJava and they work differently. The low-level API uses the `.jnew(...)`, `.jcall(...)` etc. syntax, and the high-level API uses the `J("classname")`, `object$method(arg)` syntax. The low-level API requires you to specify the class of the object returned by a method, and complains if they don't match (like if you get the JNI syntax wrong). The high-level API will just pick whatever class it deems appropriate for the returned object, so you can get something of the wrong class without realizing it.

4. rJava doesn't deal well with interfaces. It requires that you give it classes that exactly match the signature it expects. For example, the GES constructor requires a DataSet, so you have to create a ColtDataSet and then cast it to DataSet before you can create a GES instance.

5. By default, `.jcast` doesn't check whether the casting worked. You have to use `.jcast(..., check=TRUE)` if you want R to complain when the casting fails.

6. I still don't know how to determine the actual class of an object, as far as rJava is concerned. The options seem to be:

    - `dput(object)`, which prints the classname as far as R knows it (if `.jcast` fails silently, then R will _think_ it knows the class, but it will be wrong),

    - `objectName$getClass()` or `.jcall(objectName, "Ljava/lang/Class;", "getClass")`, which seem to report the instantiated class even after I have cast the object to the interface class (e.g. they say "ColtDataSet" even after I have up-cast to "DataSet"), or 

    - `objectName %instanceof% "className"`, which returns true for both the interface class and the instantiated class, regardless of whether or not I have up-cast to the interface class.

7. Basic data types dont need to be turned into Java objects, they just need to be the right types in R. So for example if your method requires an `int`, you just need to write `as.integer(2)` in R, rather than calling `.jnew("I", 2)` or anything like that. (Maybe this is obvious to people who know more CS than me.)

8. At some point I was trying to create a list of nodes, which in Java I would just initialize as `List<Node>`. I guess rJava only keeps the class of the outermost data structure, because I kept getting a `List<Object>`. It turns out this didn't matter at all; I don't know whether it will matter in future.
