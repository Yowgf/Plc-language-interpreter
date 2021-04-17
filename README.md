# Plc language interpreter

Plc is a simple ML-like functional language, that supports basic arithmetic and list operations, recursion, and high order functions. Ultimately, every expression is evaluated to a value of the type internally called PlcVal, that can be either: an integer, a boolean, a list, a sequence, or a closure.

### Try it out

Plc is built on top of SML. To run its interpreter, you need to have an SML interpreter installed in your machine. Once that is done, just run the bat or shell script "plc" (make sure you either are in the root directory or that you have it on your PATH), as in:

<pre><code>$ plc</pre></code>

or provide a script to be executed:

<pre><code>$ plc my-script.plc</pre></code>

### Examples

*Calculate the factorial of 10 recursively.* Note that there must be an expression following the ";". Different from SML, a function declaration by itself is not a valid program.

<pre><code>fun rec fat (Int n): Int = 
    if n = 0 then
        1
    else
        n * fat(n-1);
        
fat(10)
</code></pre>

*Calculate the 8th fibonacci number.*

<pre><code>fun rec fib (Int n): Int = 
    if n = 0 then
        1
    else if n = 1 then
        1
    else
        fib(n - 1) + fib(n - 2);

fib(8)
</code></pre>
