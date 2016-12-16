

# Module lru #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-cache">cache()</a> ###


<pre><code>
cache() = <a href="#type-cache">cache()</a> | <a href="#type-name">name()</a>
</code></pre>




### <a name="type-callback">callback()</a> ###


<pre><code>
callback() = function() | {function(), list()} | {module(), function(), list()}
</code></pre>




### <a name="type-lru_option">lru_option()</a> ###


<pre><code>
lru_option() = {evict_fun, function()} | {spawn_opt, list()} | {max_size, non_neg_integer()} | {max_objs, non_neg_integer()}
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = {local, Name::atom()} | {global, GlobalName::term()} | {via, ViaName::term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-3">add/3</a></td><td>adds a value to the cache.</td></tr><tr><td valign="top"><a href="#add_with-3">add_with/3</a></td><td>like add but with a callback.</td></tr><tr><td valign="top"><a href="#contains-2">contains/2</a></td><td>check if the key is in the cache.</td></tr><tr><td valign="top"><a href="#contains_or_add-3">contains_or_add/3</a></td><td> checks if a key is in the cache (without updating the recent-ness or
deleting it for being stale), if not, adds the value.</td></tr><tr><td valign="top"><a href="#contains_or_add_with-3">contains_or_add_with/3</a></td><td>like contains_or_add but with a callback.</td></tr><tr><td valign="top"><a href="#count-1">count/1</a></td><td>get the number of items in the cache.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>lookup a key's value from the cache.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>lookup a key's value from the cache.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>get cache info.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>return all the keys from the cache.</td></tr><tr><td valign="top"><a href="#peek-2">peek/2</a></td><td>Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.</td></tr><tr><td valign="top"><a href="#peek-3">peek/3</a></td><td>Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.</td></tr><tr><td valign="top"><a href="#purge-1">purge/1</a></td><td>purge all items from the cache.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>remove a key from the cache.</td></tr><tr><td valign="top"><a href="#remove_oldest-1">remove_oldest/1</a></td><td>remove the oldest item from the cache.</td></tr><tr><td valign="top"><a href="#reset_count-2">reset_count/2</a></td><td>reset the number of items in the cache.</td></tr><tr><td valign="top"><a href="#resize-2">resize/2</a></td><td>resize the cache.</td></tr><tr><td valign="top"><a href="#set_count-2">set_count/2</a></td><td>set the number of items in the cache.</td></tr><tr><td valign="top"><a href="#set_size-2">set_size/2</a></td><td>change the size of the cache.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>get the size of items in the cache.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>creates an LRU of the given size
Options are:
- <code>{evict_fun, Fun}</code> a function that will received the evicted key value
"fun(Key, Value)".</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>creates an LRU of the given size with a registered name.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>creates an LRU of the given size as part of a supervision tree.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>creates an LRU of the given size as part of a supervision tree with a
registered name.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>stop the LRU cache.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-3"></a>

### add/3 ###

<pre><code>
add(Cache::<a href="#type-cache">cache()</a>, Key::term(), Value::term() | function()) -&gt; true | false
</code></pre>
<br />

adds a value to the cache.  Returns true if an eviction occured.

<a name="add_with-3"></a>

### add_with/3 ###

<pre><code>
add_with(Cache::<a href="#type-cache">cache()</a>, Key::term(), Callback::<a href="#type-callback">callback()</a>) -&gt; true | false
</code></pre>
<br />

like add but with a callback

<a name="contains-2"></a>

### contains/2 ###

<pre><code>
contains(Cache::<a href="#type-cache">cache()</a>, Key::term() | function()) -&gt; true | false
</code></pre>
<br />

check if the key is in the cache

<a name="contains_or_add-3"></a>

### contains_or_add/3 ###

<pre><code>
contains_or_add(Cache::<a href="#type-cache">cache()</a>, Key::term(), Value::term()) -&gt; {Exists::boolean(), Evict::boolean()}
</code></pre>
<br />

checks if a key is in the cache (without updating the recent-ness or
deleting it for being stale), if not, adds the value. Returns whether found and whether an eviction
occurred.

<a name="contains_or_add_with-3"></a>

### contains_or_add_with/3 ###

<pre><code>
contains_or_add_with(Cache::<a href="#type-cache">cache()</a>, Key::term(), Callback::<a href="#type-callback">callback()</a>) -&gt; {Exists::boolean(), Evict::boolean()}
</code></pre>
<br />

like contains_or_add but with a callback

<a name="count-1"></a>

### count/1 ###

<pre><code>
count(Cache::<a href="#type-cache">cache()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

get the number of items in the cache

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Cache::<a href="#type-cache">cache()</a>, Key::term()) -&gt; term() | undefined
</code></pre>
<br />

lookup a key's value from the cache. Return undefined if it's not
found.

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Cache::<a href="#type-cache">cache()</a>, Key::term(), Default::term()) -&gt; term()
</code></pre>
<br />

lookup a key's value from the cache. Return the Default value if it's
not found.

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Cache::<a href="#type-cache">cache()</a>) -&gt; list()
</code></pre>
<br />

get cache info

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Cache::<a href="#type-cache">cache()</a>) -&gt; [term()]
</code></pre>
<br />

return all the keys from the cache

<a name="peek-2"></a>

### peek/2 ###

<pre><code>
peek(Cache::<a href="#type-cache">cache()</a>, Key::term()) -&gt; term() | undefined
</code></pre>
<br />

Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.

<a name="peek-3"></a>

### peek/3 ###

<pre><code>
peek(Cache::<a href="#type-cache">cache()</a>, Key::term(), Default::term()) -&gt; term() | undefined
</code></pre>
<br />

Returns the key value (or undefined if not found) without updating the
"recently used"-ness of the key.

<a name="purge-1"></a>

### purge/1 ###

<pre><code>
purge(Cache::<a href="#type-cache">cache()</a>) -&gt; ok
</code></pre>
<br />

purge all items from the cache.

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Cache::<a href="#type-cache">cache()</a>, Key::term()) -&gt; ok
</code></pre>
<br />

remove a key from the cache

<a name="remove_oldest-1"></a>

### remove_oldest/1 ###

<pre><code>
remove_oldest(Cache::<a href="#type-cache">cache()</a>) -&gt; ok
</code></pre>
<br />

remove the oldest item from the cache

<a name="reset_count-2"></a>

### reset_count/2 ###

`reset_count(Cache, Count) -> any()`

reset the number of items in the cache

<a name="resize-2"></a>

### resize/2 ###

<pre><code>
resize(Cache::<a href="#type-cache">cache()</a>, Size::non_neg_integer()) -&gt; ok
</code></pre>
<br />

resize the cache

<a name="set_count-2"></a>

### set_count/2 ###

<pre><code>
set_count(Cache::<a href="#type-cache">cache()</a>, Count::non_neg_integer()) -&gt; ok
</code></pre>
<br />

set the number of items in the cache

<a name="set_size-2"></a>

### set_size/2 ###

<pre><code>
set_size(Cache::<a href="#type-cache">cache()</a>, Size::non_neg_integer()) -&gt; ok
</code></pre>
<br />

change the size of the cache

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Cache::<a href="#type-cache">cache()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

get the size of items in the cache

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, <a href="#type-cache">cache()</a>} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size
Options are:
- `{evict_fun, Fun}` a function that will received the evicted key value
"fun(Key, Value)".
- `{spawn_opts, Opts}` the spawn options. see `erlang:spawn_opt/2` for
more informations.
- `{max_objs, Count}` the maximum number of items in the cache, default
is 0 for an unlimited number
- `{max_size, Size}`` the maxium size (in bytes) of items in the cache,
default is 0 for an unlimited size

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Name::<a href="#type-name">name()</a>, Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, <a href="#type-cache">cache()</a>} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size with a registered name

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, <a href="#type-cache">cache()</a>} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size as part of a supervision tree

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Name::<a href="#type-name">name()</a>, Opts::[<a href="#type-lru_option">lru_option()</a>]) -&gt; {ok, <a href="#type-cache">cache()</a>} | {error, term()}
</code></pre>
<br />

creates an LRU of the given size as part of a supervision tree with a
registered name.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Cache::<a href="#type-cache">cache()</a> | <a href="#type-name">name()</a>) -&gt; ok
</code></pre>
<br />

stop the LRU cache

