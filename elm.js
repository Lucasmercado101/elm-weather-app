(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.bL.aS === region.b3.aS)
	{
		return 'on line ' + region.bL.aS;
	}
	return 'on lines ' + region.bL.aS + ' through ' + region.b3.aS;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.en,
		impl.fA,
		impl.fd,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		V: func(record.V),
		bN: record.bN,
		bH: record.bH
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.V;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bN;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bH) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.en,
		impl.fA,
		impl.fd,
		function(sendToApp, initialModel) {
			var view = impl.fB;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.en,
		impl.fA,
		impl.fd,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bK && impl.bK(sendToApp)
			var view = impl.fB;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.dI);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.fv) && (_VirtualDom_doc.title = title = doc.fv);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.eJ;
	var onUrlRequest = impl.eK;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bK: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.cJ === next.cJ
							&& curr.cg === next.cg
							&& curr.cE.a === next.cE.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		en: function(flags)
		{
			return A3(impl.en, flags, _Browser_getUrl(), key);
		},
		fB: impl.fB,
		fA: impl.fA,
		fd: impl.fd
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { ef: 'hidden', dS: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { ef: 'mozHidden', dS: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { ef: 'msHidden', dS: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { ef: 'webkitHidden', dS: 'webkitvisibilitychange' }
		: { ef: 'hidden', dS: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		cT: _Browser_getScene(),
		c9: {
			di: _Browser_window.pageXOffset,
			dj: _Browser_window.pageYOffset,
			de: _Browser_doc.documentElement.clientWidth,
			cd: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		de: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		cd: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			cT: {
				de: node.scrollWidth,
				cd: node.scrollHeight
			},
			c9: {
				di: node.scrollLeft,
				dj: node.scrollTop,
				de: node.clientWidth,
				cd: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			cT: _Browser_getScene(),
			c9: {
				di: x,
				dj: y,
				de: _Browser_doc.documentElement.clientWidth,
				cd: _Browser_doc.documentElement.clientHeight
			},
			d6: {
				di: x + rect.left,
				dj: y + rect.top,
				de: rect.width,
				cd: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.b6.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.b6.b, xhr)); });
		$elm$core$Maybe$isJust(request.c3) && _Http_track(router, xhr, request.c3.a);

		try {
			xhr.open(request.ey, request.c5, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.c5));
		}

		_Http_configureRequest(xhr, request);

		request.dI.a && xhr.setRequestHeader('Content-Type', request.dI.a);
		xhr.send(request.dI.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.cc; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.fu.a || 0;
	xhr.responseType = request.b6.d;
	xhr.withCredentials = request.dw;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		c5: xhr.responseURL,
		e6: xhr.status,
		e7: xhr.statusText,
		cc: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			e_: event.loaded,
			cY: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			eO: event.loaded,
			cY: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}



// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.eB) { flags += 'm'; }
	if (options.dR) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.i) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.j);
		} else {
			var treeLen = builder.i * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.l) : builder.l;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.i);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.j);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{l: nodeList, i: (len / $elm$core$Array$branchFactor) | 0, j: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {ca: fragment, cg: host, cB: path, cE: port_, cJ: protocol, cK: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Api$ClearSky = 0;
var $author$project$Main$GotCountryAndStateMainScreen = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$GotRefetchingWeatherResp = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$MainScreen = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$NotRefetching = {$: 0};
var $author$project$Main$OnMainScreenMsg = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$WelcomeScreen = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Main$Flags = F2(
	function (posixTimeNow, location) {
		return {p: location, cF: posixTimeNow};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Main$flagsDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Main$Flags,
	A2($elm$json$Json$Decode$field, 'posixTimeNow', $elm$json$Json$Decode$int),
	$elm$json$Json$Decode$maybe(
		A2(
			$elm$json$Json$Decode$field,
			'location',
			A3(
				$elm$json$Json$Decode$map2,
				F2(
					function (lat, _long) {
						return {bv: lat, bx: _long};
					}),
				A2($elm$json$Json$Decode$field, 'latitude', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'longitude', $elm$json$Json$Decode$float)))));
var $justinmimbs$time_extra$Time$Extra$Day = 11;
var $justinmimbs$date$Date$Days = 3;
var $justinmimbs$time_extra$Time$Extra$Millisecond = 15;
var $justinmimbs$time_extra$Time$Extra$Month = 2;
var $justinmimbs$date$Date$Months = 1;
var $justinmimbs$date$Date$RD = $elm$core$Basics$identity;
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Basics$neq = _Utils_notEqual;
var $justinmimbs$date$Date$isLeapYear = function (y) {
	return ((!A2($elm$core$Basics$modBy, 4, y)) && (!(!A2($elm$core$Basics$modBy, 100, y)))) || (!A2($elm$core$Basics$modBy, 400, y));
};
var $justinmimbs$date$Date$daysBeforeMonth = F2(
	function (y, m) {
		var leapDays = $justinmimbs$date$Date$isLeapYear(y) ? 1 : 0;
		switch (m) {
			case 0:
				return 0;
			case 1:
				return 31;
			case 2:
				return 59 + leapDays;
			case 3:
				return 90 + leapDays;
			case 4:
				return 120 + leapDays;
			case 5:
				return 151 + leapDays;
			case 6:
				return 181 + leapDays;
			case 7:
				return 212 + leapDays;
			case 8:
				return 243 + leapDays;
			case 9:
				return 273 + leapDays;
			case 10:
				return 304 + leapDays;
			default:
				return 334 + leapDays;
		}
	});
var $justinmimbs$date$Date$floorDiv = F2(
	function (a, b) {
		return $elm$core$Basics$floor(a / b);
	});
var $justinmimbs$date$Date$daysBeforeYear = function (y1) {
	var y = y1 - 1;
	var leapYears = (A2($justinmimbs$date$Date$floorDiv, y, 4) - A2($justinmimbs$date$Date$floorDiv, y, 100)) + A2($justinmimbs$date$Date$floorDiv, y, 400);
	return (365 * y) + leapYears;
};
var $justinmimbs$date$Date$daysInMonth = F2(
	function (y, m) {
		switch (m) {
			case 0:
				return 31;
			case 1:
				return $justinmimbs$date$Date$isLeapYear(y) ? 29 : 28;
			case 2:
				return 31;
			case 3:
				return 30;
			case 4:
				return 31;
			case 5:
				return 30;
			case 6:
				return 31;
			case 7:
				return 31;
			case 8:
				return 30;
			case 9:
				return 31;
			case 10:
				return 30;
			default:
				return 31;
		}
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $justinmimbs$date$Date$monthToNumber = function (m) {
	switch (m) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$Apr = 3;
var $elm$time$Time$Aug = 7;
var $elm$time$Time$Dec = 11;
var $elm$time$Time$Feb = 1;
var $elm$time$Time$Jan = 0;
var $elm$time$Time$Jul = 6;
var $elm$time$Time$Jun = 5;
var $elm$time$Time$Mar = 2;
var $elm$time$Time$May = 4;
var $elm$time$Time$Nov = 10;
var $elm$time$Time$Oct = 9;
var $elm$time$Time$Sep = 8;
var $justinmimbs$date$Date$numberToMonth = function (mn) {
	var _v0 = A2($elm$core$Basics$max, 1, mn);
	switch (_v0) {
		case 1:
			return 0;
		case 2:
			return 1;
		case 3:
			return 2;
		case 4:
			return 3;
		case 5:
			return 4;
		case 6:
			return 5;
		case 7:
			return 6;
		case 8:
			return 7;
		case 9:
			return 8;
		case 10:
			return 9;
		case 11:
			return 10;
		default:
			return 11;
	}
};
var $justinmimbs$date$Date$toCalendarDateHelp = F3(
	function (y, m, d) {
		toCalendarDateHelp:
		while (true) {
			var monthDays = A2($justinmimbs$date$Date$daysInMonth, y, m);
			var mn = $justinmimbs$date$Date$monthToNumber(m);
			if ((mn < 12) && (_Utils_cmp(d, monthDays) > 0)) {
				var $temp$y = y,
					$temp$m = $justinmimbs$date$Date$numberToMonth(mn + 1),
					$temp$d = d - monthDays;
				y = $temp$y;
				m = $temp$m;
				d = $temp$d;
				continue toCalendarDateHelp;
			} else {
				return {d0: d, eA: m, fF: y};
			}
		}
	});
var $justinmimbs$date$Date$divWithRemainder = F2(
	function (a, b) {
		return _Utils_Tuple2(
			A2($justinmimbs$date$Date$floorDiv, a, b),
			A2($elm$core$Basics$modBy, b, a));
	});
var $justinmimbs$date$Date$year = function (_v0) {
	var rd = _v0;
	var _v1 = A2($justinmimbs$date$Date$divWithRemainder, rd, 146097);
	var n400 = _v1.a;
	var r400 = _v1.b;
	var _v2 = A2($justinmimbs$date$Date$divWithRemainder, r400, 36524);
	var n100 = _v2.a;
	var r100 = _v2.b;
	var _v3 = A2($justinmimbs$date$Date$divWithRemainder, r100, 1461);
	var n4 = _v3.a;
	var r4 = _v3.b;
	var _v4 = A2($justinmimbs$date$Date$divWithRemainder, r4, 365);
	var n1 = _v4.a;
	var r1 = _v4.b;
	var n = (!r1) ? 0 : 1;
	return ((((n400 * 400) + (n100 * 100)) + (n4 * 4)) + n1) + n;
};
var $justinmimbs$date$Date$toOrdinalDate = function (_v0) {
	var rd = _v0;
	var y = $justinmimbs$date$Date$year(rd);
	return {
		bE: rd - $justinmimbs$date$Date$daysBeforeYear(y),
		fF: y
	};
};
var $justinmimbs$date$Date$toCalendarDate = function (_v0) {
	var rd = _v0;
	var date = $justinmimbs$date$Date$toOrdinalDate(rd);
	return A3($justinmimbs$date$Date$toCalendarDateHelp, date.fF, 0, date.bE);
};
var $justinmimbs$date$Date$add = F3(
	function (unit, n, _v0) {
		var rd = _v0;
		switch (unit) {
			case 0:
				return A3($justinmimbs$date$Date$add, 1, 12 * n, rd);
			case 1:
				var date = $justinmimbs$date$Date$toCalendarDate(rd);
				var wholeMonths = ((12 * (date.fF - 1)) + ($justinmimbs$date$Date$monthToNumber(date.eA) - 1)) + n;
				var m = $justinmimbs$date$Date$numberToMonth(
					A2($elm$core$Basics$modBy, 12, wholeMonths) + 1);
				var y = A2($justinmimbs$date$Date$floorDiv, wholeMonths, 12) + 1;
				return ($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A2(
					$elm$core$Basics$min,
					date.d0,
					A2($justinmimbs$date$Date$daysInMonth, y, m));
			case 2:
				return rd + (7 * n);
			default:
				return rd + n;
		}
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $justinmimbs$date$Date$fromCalendarDate = F3(
	function (y, m, d) {
		return ($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A3(
			$elm$core$Basics$clamp,
			1,
			A2($justinmimbs$date$Date$daysInMonth, y, m),
			d);
	});
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.bL, posixMinutes) < 0) {
					return posixMinutes + era.b;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		d0: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		eA: month,
		fF: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).d0;
	});
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).eA;
		switch (_v0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
		}
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).fF;
	});
var $justinmimbs$date$Date$fromPosix = F2(
	function (zone, posix) {
		return A3(
			$justinmimbs$date$Date$fromCalendarDate,
			A2($elm$time$Time$toYear, zone, posix),
			A2($elm$time$Time$toMonth, zone, posix),
			A2($elm$time$Time$toDay, zone, posix));
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $justinmimbs$date$Date$toRataDie = function (_v0) {
	var rd = _v0;
	return rd;
};
var $justinmimbs$time_extra$Time$Extra$dateToMillis = function (date) {
	var daysSinceEpoch = $justinmimbs$date$Date$toRataDie(date) - 719163;
	return daysSinceEpoch * 86400000;
};
var $justinmimbs$time_extra$Time$Extra$timeFromClock = F4(
	function (hour, minute, second, millisecond) {
		return (((hour * 3600000) + (minute * 60000)) + (second * 1000)) + millisecond;
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMillis = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			1000,
			$elm$time$Time$posixToMillis(time));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $justinmimbs$time_extra$Time$Extra$timeFromPosix = F2(
	function (zone, posix) {
		return A4(
			$justinmimbs$time_extra$Time$Extra$timeFromClock,
			A2($elm$time$Time$toHour, zone, posix),
			A2($elm$time$Time$toMinute, zone, posix),
			A2($elm$time$Time$toSecond, zone, posix),
			A2($elm$time$Time$toMillis, zone, posix));
	});
var $justinmimbs$time_extra$Time$Extra$toOffset = F2(
	function (zone, posix) {
		var millis = $elm$time$Time$posixToMillis(posix);
		var localMillis = $justinmimbs$time_extra$Time$Extra$dateToMillis(
			A2($justinmimbs$date$Date$fromPosix, zone, posix)) + A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix);
		return ((localMillis - millis) / 60000) | 0;
	});
var $justinmimbs$time_extra$Time$Extra$posixFromDateTime = F3(
	function (zone, date, time) {
		var millis = $justinmimbs$time_extra$Time$Extra$dateToMillis(date) + time;
		var offset0 = A2(
			$justinmimbs$time_extra$Time$Extra$toOffset,
			zone,
			$elm$time$Time$millisToPosix(millis));
		var posix1 = $elm$time$Time$millisToPosix(millis - (offset0 * 60000));
		var offset1 = A2($justinmimbs$time_extra$Time$Extra$toOffset, zone, posix1);
		if (_Utils_eq(offset0, offset1)) {
			return posix1;
		} else {
			var posix2 = $elm$time$Time$millisToPosix(millis - (offset1 * 60000));
			var offset2 = A2($justinmimbs$time_extra$Time$Extra$toOffset, zone, posix2);
			return _Utils_eq(offset1, offset2) ? posix2 : posix1;
		}
	});
var $justinmimbs$time_extra$Time$Extra$add = F4(
	function (interval, n, zone, posix) {
		add:
		while (true) {
			switch (interval) {
				case 15:
					return $elm$time$Time$millisToPosix(
						$elm$time$Time$posixToMillis(posix) + n);
				case 14:
					var $temp$interval = 15,
						$temp$n = n * 1000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 13:
					var $temp$interval = 15,
						$temp$n = n * 60000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 12:
					var $temp$interval = 15,
						$temp$n = n * 3600000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 11:
					return A3(
						$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							$justinmimbs$date$Date$add,
							3,
							n,
							A2($justinmimbs$date$Date$fromPosix, zone, posix)),
						A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 2:
					return A3(
						$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							$justinmimbs$date$Date$add,
							1,
							n,
							A2($justinmimbs$date$Date$fromPosix, zone, posix)),
						A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 0:
					var $temp$interval = 2,
						$temp$n = n * 12,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 1:
					var $temp$interval = 2,
						$temp$n = n * 3,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 3:
					var $temp$interval = 11,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				default:
					var weekday = interval;
					var $temp$interval = 11,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
			}
		}
	});
var $author$project$Api$endpoint = function (_v0) {
	var latitude = _v0.bv;
	var longitude = _v0.bx;
	var startDate = _v0.bM;
	var endDate = _v0.bq;
	return 'https://api.open-meteo.com/v1/forecast?latitude=' + (latitude + ('&longitude=' + (longitude + ('&hourly=temperature_2m,relativehumidity_2m,apparent_temperature,weathercode,visibility,windspeed_10m&daily=weathercode,temperature_2m_max&timezone=auto&start_date=' + (startDate + ('&end_date=' + (endDate + '')))))));
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.e6));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {cN: reqs, c_: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.c3;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.cN));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.c_)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					dw: r.dw,
					dI: r.dI,
					b6: A2(_Http_mapExpect, func, r.b6),
					cc: r.cc,
					ey: r.ey,
					fu: r.fu,
					c3: r.c3,
					c5: r.c5
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{dw: false, dI: r.dI, b6: r.b6, cc: r.cc, ey: r.ey, fu: r.fu, c3: r.c3, c5: r.c5}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{dI: $elm$http$Http$emptyBody, b6: r.b6, cc: _List_Nil, ey: 'GET', fu: $elm$core$Maybe$Nothing, c3: $elm$core$Maybe$Nothing, c5: r.c5});
};
var $author$project$Utils$monthToInt = function (month) {
	switch (month) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 12;
	}
};
var $justinmimbs$time_extra$Time$Extra$posixToParts = F2(
	function (zone, posix) {
		return {
			d0: A2($elm$time$Time$toDay, zone, posix),
			bu: A2($elm$time$Time$toHour, zone, posix),
			bz: A2($elm$time$Time$toMillis, zone, posix),
			bB: A2($elm$time$Time$toMinute, zone, posix),
			eA: A2($elm$time$Time$toMonth, zone, posix),
			bI: A2($elm$time$Time$toSecond, zone, posix),
			fF: A2($elm$time$Time$toYear, zone, posix)
		};
	});
var $author$project$Api$ResponseData = F2(
	function (daily, hourly) {
		return {d$: daily, eg: hourly};
	});
var $author$project$Api$DenseDrizzle = 8;
var $author$project$Api$DenseFreezingDrizzle = 10;
var $author$project$Api$Fog = 4;
var $author$project$Api$HeavyFreezingRain = 15;
var $author$project$Api$HeavyRain = 13;
var $author$project$Api$HeavySnowFall = 18;
var $author$project$Api$HeavySnowShowers = 24;
var $author$project$Api$HeavyThunderstormWithHail = 27;
var $author$project$Api$LightDrizzle = 6;
var $author$project$Api$LightFreezingDrizzle = 9;
var $author$project$Api$LightFreezingRain = 14;
var $author$project$Api$MainlyClear = 1;
var $author$project$Api$ModerateDrizzle = 7;
var $author$project$Api$ModerateRain = 12;
var $author$project$Api$ModerateRainShowers = 21;
var $author$project$Api$ModerateSnowFall = 17;
var $author$project$Api$Overcast = 3;
var $author$project$Api$RimeFog = 5;
var $author$project$Api$SlightOrModerateThunderstorm = 25;
var $author$project$Api$SlightRain = 11;
var $author$project$Api$SlightRainShowers = 20;
var $author$project$Api$SlightSnowFall = 16;
var $author$project$Api$SlightSnowShowers = 23;
var $author$project$Api$SlightThunderstormWithHail = 26;
var $author$project$Api$SnowGrains = 19;
var $author$project$Api$ViolentRainShowers = 22;
var $author$project$Api$codeToDescription = function (code) {
	switch (code) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 1;
		case 3:
			return 3;
		case 45:
			return 4;
		case 48:
			return 5;
		case 51:
			return 6;
		case 53:
			return 7;
		case 55:
			return 8;
		case 56:
			return 9;
		case 57:
			return 10;
		case 61:
			return 11;
		case 63:
			return 12;
		case 65:
			return 13;
		case 66:
			return 14;
		case 67:
			return 15;
		case 71:
			return 16;
		case 73:
			return 17;
		case 75:
			return 18;
		case 77:
			return 19;
		case 80:
			return 20;
		case 81:
			return 21;
		case 82:
			return 22;
		case 85:
			return 23;
		case 86:
			return 24;
		case 95:
			return 25;
		case 96:
			return 26;
		case 99:
			return 27;
		default:
			return 0;
	}
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$core$List$map3 = _List_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
				}
			}
		};
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {b1: col, dX: contextStack, cG: problem, cR: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.cR, s.b1, x, s.d));
	});
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.a),
			s.b) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{b1: col, d: s0.d, e: s0.e, b: offset, cR: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.cR, s.b1, s);
	};
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.b, s1.b, s0.a),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$core$Basics$round = _Basics_round;
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$core$String$toFloat = _String_toFloat;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		if ($elm$core$String$length(str) <= 9) {
			var _v0 = $elm$core$String$toFloat('0.' + str);
			if (!_v0.$) {
				var floatVal = _v0.a;
				return $elm$parser$Parser$succeed(
					$elm$core$Basics$round(floatVal * 1000));
			} else {
				return $elm$parser$Parser$problem('Invalid float: \"' + (str + '\"'));
			}
		} else {
			return $elm$parser$Parser$problem(
				'Expected at most 9 digits, but got ' + $elm$core$String$fromInt(
					$elm$core$String$length(str)));
		}
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile($elm$core$Char$isDigit)));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts = F6(
	function (monthYearDayMs, hour, minute, second, ms, utcOffsetMinutes) {
		return $elm$time$Time$millisToPosix((((monthYearDayMs + (((hour * 60) * 60) * 1000)) + (((minute - utcOffsetMinutes) * 60) * 1000)) + (second * 1000)) + ms);
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$append = _String_append;
var $elm$parser$Parser$UnexpectedChar = {$: 11};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.b, s.a);
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{b1: 1, d: s.d, e: s.e, b: s.b + 1, cR: s.cR + 1, a: s.a}) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{b1: s.b1 + 1, d: s.d, e: s.e, b: newOffset, cR: s.cR, a: s.a}));
		};
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt = function (quantity) {
	var helper = function (str) {
		if (_Utils_eq(
			$elm$core$String$length(str),
			quantity)) {
			var _v0 = $elm$core$String$toInt(str);
			if (!_v0.$) {
				var intVal = _v0.a;
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$Done,
					$elm$parser$Parser$succeed(intVal));
			} else {
				return $elm$parser$Parser$problem('Invalid integer: \"' + (str + '\"'));
			}
		} else {
			return A2(
				$elm$parser$Parser$map,
				function (nextChar) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$String$append, str, nextChar));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
		}
	};
	return A2($elm$parser$Parser$loop, '', helper);
};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.cR, s.b1, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{b1: newCol, d: s.d, e: s.e, b: newOffset, cR: newRow, a: s.a});
	};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear = 1970;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay = function (day) {
	return $elm$parser$Parser$problem(
		'Invalid day: ' + $elm$core$String$fromInt(day));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear = function (year) {
	return (!A2($elm$core$Basics$modBy, 4, year)) && ((!(!A2($elm$core$Basics$modBy, 100, year))) || (!A2($elm$core$Basics$modBy, 400, year)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore = function (y1) {
	var y = y1 - 1;
	return (((y / 4) | 0) - ((y / 100) | 0)) + ((y / 400) | 0);
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay = 86400000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear = 31536000000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay = function (_v0) {
	var year = _v0.a;
	var month = _v0.b;
	var dayInMonth = _v0.c;
	if (dayInMonth < 0) {
		return $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth);
	} else {
		var succeedWith = function (extraMs) {
			var yearMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear * (year - $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear);
			var days = ((month < 3) || (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year))) ? (dayInMonth - 1) : dayInMonth;
			var dayMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay * (days + ($rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore(year) - $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore($rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear)));
			return $elm$parser$Parser$succeed((extraMs + yearMs) + dayMs);
		};
		switch (month) {
			case 1:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(0);
			case 2:
				return ((dayInMonth > 29) || ((dayInMonth === 29) && (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year)))) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(2678400000);
			case 3:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(5097600000);
			case 4:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(7776000000);
			case 5:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(10368000000);
			case 6:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(13046400000);
			case 7:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(15638400000);
			case 8:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(18316800000);
			case 9:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(20995200000);
			case 10:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(23587200000);
			case 11:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(26265600000);
			case 12:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(28857600000);
			default:
				return $elm$parser$Parser$problem(
					'Invalid month: \"' + ($elm$core$String$fromInt(month) + '\"'));
		}
	}
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs = A2(
	$elm$parser$Parser$andThen,
	$rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F3(
						function (year, month, day) {
							return _Utils_Tuple3(year, month, day);
						})),
				$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(4)),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($elm$core$Basics$identity),
							$elm$parser$Parser$symbol('-')),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
					]))),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($elm$core$Basics$identity),
						$elm$parser$Parser$symbol('-')),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
				]))));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes = function () {
	var utcOffsetMinutesFromParts = F3(
		function (multiplier, hours, minutes) {
			return (multiplier * (hours * 60)) + minutes;
		});
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return 0;
					},
					$elm$parser$Parser$symbol('Z')),
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(utcOffsetMinutesFromParts),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$map,
										function (_v1) {
											return 1;
										},
										$elm$parser$Parser$symbol('+')),
										A2(
										$elm$parser$Parser$map,
										function (_v2) {
											return -1;
										},
										$elm$parser$Parser$symbol('-'))
									]))),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed($elm$core$Basics$identity),
									$elm$parser$Parser$symbol(':')),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
								$elm$parser$Parser$succeed(0)
							]))),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$end)
				])));
}();
var $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601 = A2(
	$elm$parser$Parser$andThen,
	function (datePart) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											$rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts(datePart)),
										$elm$parser$Parser$symbol('T')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$elm$parser$Parser$oneOf(
									_List_fromArray(
										[
											A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$ignorer,
												$elm$parser$Parser$succeed($elm$core$Basics$identity),
												$elm$parser$Parser$symbol(':')),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
										]))),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$ignorer,
											$elm$parser$Parser$succeed($elm$core$Basics$identity),
											$elm$parser$Parser$symbol(':')),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
										$elm$parser$Parser$succeed(0)
									]))),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$symbol('.')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs),
									$elm$parser$Parser$succeed(0)
								]))),
					A2($elm$parser$Parser$ignorer, $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes, $elm$parser$Parser$end)),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A6($rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts, datePart, 0, 0, 0, 0, 0)),
					$elm$parser$Parser$end)
				]));
	},
	$rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs);
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {b1: col, cG: problem, cR: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.cR, p.b1, p.cG);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{b1: 1, d: _List_Nil, e: 1, b: 0, cR: 1, a: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime = function (str) {
	return A2($elm$parser$Parser$run, $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601, str);
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Api$timesToPosix = function (timesStr) {
	return A2(
		$elm$core$List$map,
		$elm$core$Result$withDefault(
			$elm$time$Time$millisToPosix(0)),
		A2($elm$core$List$map, $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime, timesStr));
};
var $author$project$Api$dailyDecoder = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (times, weatherCodes, maxWeather) {
			var wmoCodes = A2($elm$core$List$map, $author$project$Api$codeToDescription, weatherCodes);
			return A4(
				$elm$core$List$map3,
				F3(
					function (t, w, m) {
						return _Utils_Tuple3(t, w, m);
					}),
				times,
				wmoCodes,
				maxWeather);
		}),
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Api$timesToPosix,
		A2(
			$elm$json$Json$Decode$field,
			'time',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
	A2(
		$elm$json$Json$Decode$field,
		'weathercode',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$field,
		'temperature_2m_max',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$float)));
var $author$project$Api$listMap7 = F8(
	function (fn, a, b, c, d, e, f, g) {
		var _v0 = _Utils_Tuple3(a, b, c);
		if ((_v0.a.b && _v0.b.b) && _v0.c.b) {
			var _v1 = _v0.a;
			var x = _v1.a;
			var xs = _v1.b;
			var _v2 = _v0.b;
			var y = _v2.a;
			var ys = _v2.b;
			var _v3 = _v0.c;
			var z = _v3.a;
			var zs = _v3.b;
			var _v4 = _Utils_Tuple3(d, e, f);
			if ((_v4.a.b && _v4.b.b) && _v4.c.b) {
				var _v5 = _v4.a;
				var xd = _v5.a;
				var ds = _v5.b;
				var _v6 = _v4.b;
				var xe = _v6.a;
				var es = _v6.b;
				var _v7 = _v4.c;
				var xf = _v7.a;
				var fs = _v7.b;
				if (g.b) {
					var xg = g.a;
					var gs = g.b;
					return A2(
						$elm$core$List$cons,
						A7(fn, x, y, z, xd, xe, xf, xg),
						A8($author$project$Api$listMap7, fn, xs, ys, zs, ds, es, fs, gs));
				} else {
					return _List_Nil;
				}
			} else {
				return _List_Nil;
			}
		} else {
			return _List_Nil;
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$json$Json$Decode$map7 = _Json_map7;
var $author$project$Api$hourlyDecoder = A8(
	$elm$json$Json$Decode$map7,
	F7(
		function (times, temp, humidity, apparentTemp, weatherCode, windSpeed, visibility) {
			return A8(
				$author$project$Api$listMap7,
				F7(
					function (time, tem, h, at, weatherC, ws, v) {
						var hourly = {
							bY: at,
							cL: h,
							c0: tem,
							c2: time,
							da: v,
							db: A2($elm$core$Maybe$map, $author$project$Api$codeToDescription, weatherC),
							dh: ws
						};
						return hourly;
					}),
				times,
				temp,
				humidity,
				apparentTemp,
				weatherCode,
				windSpeed,
				visibility);
		}),
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Api$timesToPosix,
		A2(
			$elm$json$Json$Decode$field,
			'time',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
	A2(
		$elm$json$Json$Decode$field,
		'temperature_2m',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$maybe($elm$json$Json$Decode$float))),
	A2(
		$elm$json$Json$Decode$field,
		'relativehumidity_2m',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$field,
		'apparent_temperature',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$maybe($elm$json$Json$Decode$float))),
	A2(
		$elm$json$Json$Decode$field,
		'weathercode',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int))),
	A2(
		$elm$json$Json$Decode$field,
		'windspeed_10m',
		$elm$json$Json$Decode$list(
			$elm$json$Json$Decode$maybe($elm$json$Json$Decode$float))),
	A2(
		$elm$json$Json$Decode$field,
		'visibility',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$float)));
var $author$project$Api$responseDataDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Api$ResponseData,
	A2($elm$json$Json$Decode$field, 'daily', $author$project$Api$dailyDecoder),
	A2($elm$json$Json$Decode$field, 'hourly', $author$project$Api$hourlyDecoder));
var $author$project$Api$getData = F4(
	function (_v0, startDate, zone, msg) {
		var lat = _v0.a;
		var _long = _v0.b;
		var longitude = $elm$core$String$fromFloat(_long);
		var latitude = $elm$core$String$fromFloat(lat);
		var endDate = A4($justinmimbs$time_extra$Time$Extra$add, 11, 7, zone, startDate);
		var dateToStr = function (parts) {
			return $elm$core$String$fromInt(parts.fF) + ('-' + (function (l) {
				return ($elm$core$String$length(l) === 1) ? ('0' + l) : l;
			}(
				$elm$core$String$fromInt(
					$author$project$Utils$monthToInt(parts.eA))) + ('-' + function (l) {
				return ($elm$core$String$length(l) === 1) ? ('0' + l) : l;
			}(
				$elm$core$String$fromInt(parts.d0)))));
		};
		var endDateStrings = dateToStr(
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, endDate));
		var startDateString = dateToStr(
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, startDate));
		return $elm$http$Http$get(
			{
				b6: A2($elm$http$Http$expectJson, msg, $author$project$Api$responseDataDecoder),
				c5: $author$project$Api$endpoint(
					{bq: endDateStrings, bv: latitude, bx: longitude, bM: startDateString})
			});
	});
var $author$project$Api$ReverseGeocodingResponse = function (address) {
	return {dm: address};
};
var $author$project$Api$Address = F2(
	function (country, state) {
		return {dY: country, e5: state};
	});
var $author$project$Api$addressDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Api$Address,
	A2($elm$json$Json$Decode$field, 'country', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'state', $elm$json$Json$Decode$string));
var $author$project$Api$reverseGeocodingDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Api$ReverseGeocodingResponse,
	A2($elm$json$Json$Decode$field, 'address', $author$project$Api$addressDecoder));
var $author$project$Api$reverseGeocodingEndpoint = function (_v0) {
	var lat = _v0.a;
	var _long = _v0.b;
	return 'https://nominatim.openstreetmap.org/reverse?lat=' + ($elm$core$String$fromFloat(lat) + ('&lon=' + ($elm$core$String$fromFloat(_long) + ('&format=json' + '&zoom=8'))));
};
var $author$project$Api$getReverseGeocoding = F2(
	function (_v0, msg) {
		var lat = _v0.a;
		var _long = _v0.b;
		return $elm$http$Http$get(
			{
				b6: A2($elm$http$Http$expectJson, msg, $author$project$Api$reverseGeocodingDecoder),
				c5: $author$project$Api$reverseGeocodingEndpoint(
					_Utils_Tuple2(lat, _long))
			});
	});
var $mdgriffith$elm_animator$Internal$Timeline$Timeline = $elm$core$Basics$identity;
var $mdgriffith$elm_animator$Internal$Timeline$Timetable = $elm$core$Basics$identity;
var $ianmackenzie$elm_units$Quantity$Quantity = $elm$core$Basics$identity;
var $mdgriffith$elm_animator$Internal$Time$absolute = function (posix) {
	return $elm$time$Time$posixToMillis(posix);
};
var $mdgriffith$elm_animator$Animator$init = function (first) {
	return {
		b5: _List_Nil,
		cj: first,
		aQ: _List_Nil,
		cu: $mdgriffith$elm_animator$Internal$Time$absolute(
			$elm$time$Time$millisToPosix(0)),
		aU: $elm$core$Maybe$Nothing,
		bh: true
	};
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var $author$project$Utils$primary = A3($mdgriffith$elm_ui$Element$rgb255, 254, 225, 66);
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Screens$Welcome$welcomeScreenInit = function (posixTimeNow) {
	return {
		f: $elm$time$Time$millisToPosix(posixTimeNow),
		d7: $elm$core$Maybe$Nothing,
		aC: _Utils_Tuple2('', ''),
		ag: '',
		eP: $elm$core$Maybe$Nothing
	};
};
var $author$project$Main$init = function (val) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $author$project$Main$flagsDecoder, val);
	if (!_v0.$) {
		var posixTimeNow = _v0.a.cF;
		var location = _v0.a.p;
		if (!location.$) {
			var latitude = location.a.bv;
			var longitude = location.a.bx;
			return _Utils_Tuple2(
				$author$project$Main$MainScreen(
					{
						B: {
							d$: _List_fromArray(
								[
									_Utils_Tuple3(
									$elm$time$Time$millisToPosix(0),
									0,
									15)
								]),
							eg: _List_fromArray(
								[
									{
									bY: $elm$core$Maybe$Just(15),
									cL: 15,
									c0: $elm$core$Maybe$Just(52),
									c2: $elm$time$Time$millisToPosix(1676938588246),
									da: 15,
									db: $elm$core$Maybe$Just(0),
									dh: $elm$core$Maybe$Just(13)
								},
									{
									bY: $elm$core$Maybe$Just(2),
									cL: 10,
									c0: $elm$core$Maybe$Just(15),
									c2: $elm$time$Time$millisToPosix(1676938582246),
									da: 14,
									db: $elm$core$Maybe$Just(0),
									dh: $elm$core$Maybe$Just(13)
								}
								])
						},
						dY: '',
						F: $mdgriffith$elm_animator$Animator$init(false),
						x: $mdgriffith$elm_animator$Animator$init($author$project$Main$NotRefetching),
						ab: $author$project$Main$NotRefetching,
						f: $elm$time$Time$millisToPosix(posixTimeNow),
						aB: false,
						p: _Utils_Tuple2(latitude, longitude),
						t: $author$project$Utils$primary,
						e5: '',
						k: $elm$time$Time$utc
					}),
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							A2(
							$elm$core$Platform$Cmd$map,
							$author$project$Main$OnMainScreenMsg,
							A4(
								$author$project$Api$getData,
								_Utils_Tuple2(latitude, longitude),
								$elm$time$Time$millisToPosix(posixTimeNow),
								$elm$time$Time$utc,
								$author$project$Main$GotRefetchingWeatherResp)),
							A2(
							$elm$core$Platform$Cmd$map,
							$author$project$Main$OnMainScreenMsg,
							A2(
								$author$project$Api$getReverseGeocoding,
								_Utils_Tuple2(latitude, longitude),
								$author$project$Main$GotCountryAndStateMainScreen))
						])));
		} else {
			return _Utils_Tuple2(
				$author$project$Main$WelcomeScreen(
					$author$project$Screens$Welcome$welcomeScreenInit(posixTimeNow)),
				$elm$core$Platform$Cmd$none);
		}
	} else {
		var err = _v0.a;
		return _Utils_Tuple2(
			$author$project$Main$WelcomeScreen(
				$author$project$Screens$Welcome$welcomeScreenInit(0)),
			$elm$core$Platform$Cmd$none);
	}
};
var $author$project$Main$OnWelcomeScreenMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$Tick = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_animator$Internal$Timeline$Animator = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_animator$Animator$animator = A2(
	$mdgriffith$elm_animator$Internal$Timeline$Animator,
	$elm$core$Basics$always(false),
	F2(
		function (now, model) {
			return model;
		}));
var $mdgriffith$elm_animator$Internal$Timeline$Line = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_animator$Internal$Timeline$Occurring = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $ianmackenzie$elm_units$Duration$inSeconds = function (_v0) {
	var numSeconds = _v0;
	return numSeconds;
};
var $ianmackenzie$elm_units$Duration$inMilliseconds = function (duration) {
	return $ianmackenzie$elm_units$Duration$inSeconds(duration) * 1000;
};
var $ianmackenzie$elm_units$Quantity$plus = F2(
	function (_v0, _v1) {
		var y = _v0;
		var x = _v1;
		return x + y;
	});
var $mdgriffith$elm_animator$Internal$Time$advanceBy = F2(
	function (dur, time) {
		return A2(
			$ianmackenzie$elm_units$Quantity$plus,
			time,
			$ianmackenzie$elm_units$Duration$inMilliseconds(dur));
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $mdgriffith$elm_animator$Internal$Timeline$toOccurring = F2(
	function (_v0, _v1) {
		var duration = _v0.a;
		var event = _v0.b;
		var maybeDwell = _v0.c;
		var now = _v1.a;
		var events = _v1.b;
		var occursAt = A2($mdgriffith$elm_animator$Internal$Time$advanceBy, duration, now);
		var endsAt = function () {
			if (maybeDwell.$ === 1) {
				return occursAt;
			} else {
				var dwell = maybeDwell.a;
				return A2($mdgriffith$elm_animator$Internal$Time$advanceBy, dwell, occursAt);
			}
		}();
		return _Utils_Tuple2(
			endsAt,
			A2(
				$elm$core$List$cons,
				A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, event, occursAt, endsAt),
				events));
	});
var $mdgriffith$elm_animator$Internal$Timeline$createLine = F2(
	function (now, scheduled) {
		var _v0 = scheduled.b;
		var dur = _v0.a;
		var startEvent = _v0.b;
		var maybeDwell = _v0.c;
		var reverseQueued = scheduled.c;
		var start = A2($mdgriffith$elm_animator$Internal$Time$advanceBy, dur, now);
		var startNextEvent = function () {
			if (maybeDwell.$ === 1) {
				return start;
			} else {
				var dwell = maybeDwell.a;
				return A2($mdgriffith$elm_animator$Internal$Time$advanceBy, dwell, start);
			}
		}();
		var events = $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_animator$Internal$Timeline$toOccurring,
				_Utils_Tuple2(startNextEvent, _List_Nil),
				$elm$core$List$reverse(reverseQueued)).b);
		return A3(
			$mdgriffith$elm_animator$Internal$Timeline$Line,
			now,
			A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, startEvent, start, startNextEvent),
			events);
	});
var $mdgriffith$elm_animator$Internal$Timeline$endTime = function (_v0) {
	var end = _v0.c;
	return end;
};
var $mdgriffith$elm_animator$Internal$Time$latest = F2(
	function (oneQty, twoQty) {
		var one = oneQty;
		var two = twoQty;
		return ((one - two) <= 0) ? twoQty : oneQty;
	});
var $mdgriffith$elm_animator$Internal$Time$thisAfterThat = F2(
	function (_v0, _v1) {
		var _this = _v0;
		var that = _v1;
		return (_this - that) > 0;
	});
var $mdgriffith$elm_animator$Internal$Timeline$addEventsToLine = F4(
	function (now, scheduled, existing, lines) {
		var delay = scheduled.a;
		var scheduledStartingEvent = scheduled.b;
		var reverseQueued = scheduled.c;
		var startLineAt = existing.a;
		var startingEvent = existing.b;
		var events = existing.c;
		var start = A2($mdgriffith$elm_animator$Internal$Time$advanceBy, delay, now);
		var _v0 = $elm$core$List$reverse(events);
		if (!_v0.b) {
			var startingEventWithDwell = function () {
				var ev = startingEvent.a;
				var lastEventTime = startingEvent.b;
				return A2($mdgriffith$elm_animator$Internal$Time$thisAfterThat, start, lastEventTime) ? A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, ev, lastEventTime, start) : A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, ev, lastEventTime, lastEventTime);
			}();
			var startNewEventsAt = A2(
				$mdgriffith$elm_animator$Internal$Time$latest,
				A2(
					$mdgriffith$elm_animator$Internal$Time$advanceBy,
					delay,
					$mdgriffith$elm_animator$Internal$Timeline$endTime(startingEvent)),
				start);
			var newLine = A2($mdgriffith$elm_animator$Internal$Timeline$createLine, startNewEventsAt, scheduled);
			return A2(
				$elm$core$List$cons,
				A3($mdgriffith$elm_animator$Internal$Timeline$Line, startLineAt, startingEventWithDwell, _List_Nil),
				A2($elm$core$List$cons, newLine, lines));
		} else {
			var _v2 = _v0.a;
			var lastEvent = _v2.a;
			var lastEventTime = _v2.b;
			var lastEventFinish = _v2.c;
			var eventTail = _v0.b;
			var startNewEventsAt = A2(
				$mdgriffith$elm_animator$Internal$Time$latest,
				A2($mdgriffith$elm_animator$Internal$Time$advanceBy, delay, lastEventFinish),
				start);
			var newLine = A2($mdgriffith$elm_animator$Internal$Timeline$createLine, startNewEventsAt, scheduled);
			var newLastEvent = A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, lastEvent, lastEventTime, startNewEventsAt);
			return A2(
				$elm$core$List$cons,
				A3(
					$mdgriffith$elm_animator$Internal$Timeline$Line,
					startLineAt,
					startingEvent,
					$elm$core$List$reverse(
						A2($elm$core$List$cons, newLastEvent, eventTail))),
				A2($elm$core$List$cons, newLine, lines));
		}
	});
var $mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat = F2(
	function (_v0, _v1) {
		var _this = _v0;
		var that = _v1;
		return (_this - that) >= 0;
	});
var $mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat = F2(
	function (_v0, _v1) {
		var _this = _v0;
		var that = _v1;
		return (_this - that) <= 0;
	});
var $mdgriffith$elm_animator$Internal$Timeline$addToCurrentLine = F3(
	function (now, scheduled, lines) {
		if (!lines.b) {
			return _List_fromArray(
				[
					A2($mdgriffith$elm_animator$Internal$Timeline$createLine, now, scheduled)
				]);
		} else {
			if (!lines.b.b) {
				var line = lines.a;
				return A4($mdgriffith$elm_animator$Internal$Timeline$addEventsToLine, now, scheduled, line, _List_Nil);
			} else {
				var _v1 = lines.a;
				var startOne = _v1.a;
				var startEventOne = _v1.b;
				var one = _v1.c;
				var _v2 = lines.b;
				var _v3 = _v2.a;
				var startTwo = _v3.a;
				var startEventTwo = _v3.b;
				var two = _v3.c;
				var remaining = _v2.b;
				return (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, now, startOne) && A2($mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat, now, startTwo)) ? A4(
					$mdgriffith$elm_animator$Internal$Timeline$addEventsToLine,
					now,
					scheduled,
					A3($mdgriffith$elm_animator$Internal$Timeline$Line, startOne, startEventOne, one),
					A2(
						$elm$core$List$cons,
						A3($mdgriffith$elm_animator$Internal$Timeline$Line, startTwo, startEventTwo, two),
						remaining)) : A2(
					$elm$core$List$cons,
					A3($mdgriffith$elm_animator$Internal$Timeline$Line, startOne, startEventOne, one),
					A3(
						$mdgriffith$elm_animator$Internal$Timeline$addToCurrentLine,
						now,
						scheduled,
						A2(
							$elm$core$List$cons,
							A3($mdgriffith$elm_animator$Internal$Timeline$Line, startTwo, startEventTwo, two),
							remaining)));
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$enqueue = F3(
	function (timeline, now, scheduled) {
		var _v0 = timeline.b5;
		var lines = _v0;
		return A3($mdgriffith$elm_animator$Internal$Timeline$addToCurrentLine, now, scheduled, lines);
	});
var $mdgriffith$elm_animator$Internal$Timeline$LastTwoEvents = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_animator$Internal$Time$thisBeforeThat = F2(
	function (_v0, _v1) {
		var _this = _v0;
		var that = _v1;
		return (_this - that) < 0;
	});
var $mdgriffith$elm_animator$Internal$Timeline$beforeEventEnd = F2(
	function (time, events) {
		beforeEventEnd:
		while (true) {
			if (!events.b) {
				return false;
			} else {
				var top = events.a;
				var remain = events.b;
				if (A2(
					$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
					time,
					$mdgriffith$elm_animator$Internal$Timeline$endTime(top))) {
					return true;
				} else {
					var $temp$time = time,
						$temp$events = remain;
					time = $temp$time;
					events = $temp$events;
					continue beforeEventEnd;
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$beforeLineEnd = F2(
	function (time, _v0) {
		var lineStartAt = _v0.a;
		var startingEvent = _v0.b;
		var trailing = _v0.c;
		if (A2($mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat, time, lineStartAt)) {
			return true;
		} else {
			if (!trailing.b) {
				return A2(
					$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
					time,
					$mdgriffith$elm_animator$Internal$Timeline$endTime(startingEvent));
			} else {
				return A2($mdgriffith$elm_animator$Internal$Timeline$beforeEventEnd, time, trailing);
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$getEvent = function (_v0) {
	var ev = _v0.a;
	return ev;
};
var $mdgriffith$elm_animator$Internal$Timeline$startTime = function (_v0) {
	var time = _v0.b;
	return time;
};
var $mdgriffith$elm_animator$Internal$Timeline$getTransitionAt = F3(
	function (interruptionTime, prev, trailing) {
		getTransitionAt:
		while (true) {
			if (!trailing.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var next = trailing.a;
				var remain = trailing.b;
				if (A2(
					$mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat,
					interruptionTime,
					$mdgriffith$elm_animator$Internal$Timeline$endTime(prev)) && A2(
					$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
					interruptionTime,
					$mdgriffith$elm_animator$Internal$Timeline$startTime(next))) {
					return $elm$core$Maybe$Just(
						A4(
							$mdgriffith$elm_animator$Internal$Timeline$LastTwoEvents,
							$mdgriffith$elm_animator$Internal$Timeline$endTime(prev),
							$mdgriffith$elm_animator$Internal$Timeline$getEvent(prev),
							$mdgriffith$elm_animator$Internal$Timeline$startTime(next),
							$mdgriffith$elm_animator$Internal$Timeline$getEvent(next)));
				} else {
					var $temp$interruptionTime = interruptionTime,
						$temp$prev = next,
						$temp$trailing = remain;
					interruptionTime = $temp$interruptionTime;
					prev = $temp$prev;
					trailing = $temp$trailing;
					continue getTransitionAt;
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$Schedule = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_animator$Internal$Timeline$Event = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_animator$Internal$Timeline$adjustScheduledDuration = F2(
	function (fn, _v0) {
		var dur = _v0.a;
		var ev = _v0.b;
		var maybeDwell = _v0.c;
		return A3(
			$mdgriffith$elm_animator$Internal$Timeline$Event,
			fn(dur),
			ev,
			maybeDwell);
	});
var $mdgriffith$elm_animator$Internal$Timeline$getScheduledEvent = function (_v0) {
	var ev = _v0.b;
	return ev;
};
var $ianmackenzie$elm_units$Quantity$multiplyBy = F2(
	function (scale, _v0) {
		var value = _v0;
		return scale * value;
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $mdgriffith$elm_animator$Internal$Time$progress = F3(
	function (_v0, _v1, _v2) {
		var start = _v0;
		var end = _v1;
		var current = _v2;
		var total = $elm$core$Basics$abs(end - start);
		return (!total) ? 0 : A2(
			$elm$core$Basics$min,
			1,
			A2($elm$core$Basics$max, 0, (current - start) / total));
	});
var $mdgriffith$elm_animator$Internal$Timeline$interruptAtExactly = F3(
	function (startInterruption, scheduled, last) {
		var penultimateTime = last.a;
		var penultimate = last.b;
		var lastEventTime = last.c;
		var lastEvent = last.d;
		var delay_ = scheduled.a;
		var startingEvent = scheduled.b;
		var reverseQueued = scheduled.c;
		var amountProgress = A3($mdgriffith$elm_animator$Internal$Time$progress, penultimateTime, lastEventTime, startInterruption);
		var newStartingEvent = _Utils_eq(
			penultimate,
			$mdgriffith$elm_animator$Internal$Timeline$getScheduledEvent(startingEvent)) ? A2(
			$mdgriffith$elm_animator$Internal$Timeline$adjustScheduledDuration,
			$ianmackenzie$elm_units$Quantity$multiplyBy(amountProgress),
			startingEvent) : startingEvent;
		return A2(
			$mdgriffith$elm_animator$Internal$Timeline$createLine,
			startInterruption,
			A3($mdgriffith$elm_animator$Internal$Timeline$Schedule, delay_, newStartingEvent, reverseQueued));
	});
var $mdgriffith$elm_animator$Internal$Timeline$interruptLine = F4(
	function (startInterruption, scheduled, line, future) {
		var start = line.a;
		var startEvent = line.b;
		var trailing = line.c;
		if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, startInterruption, start)) {
			if (!future.b) {
				var _v2 = A3($mdgriffith$elm_animator$Internal$Timeline$getTransitionAt, startInterruption, startEvent, trailing);
				if (_v2.$ === 1) {
					return A2($mdgriffith$elm_animator$Internal$Timeline$beforeLineEnd, startInterruption, line) ? $elm$core$Maybe$Just(
						_List_fromArray(
							[
								A2($mdgriffith$elm_animator$Internal$Timeline$createLine, startInterruption, scheduled)
							])) : $elm$core$Maybe$Nothing;
				} else {
					var last2Events = _v2.a;
					return $elm$core$Maybe$Just(
						_List_fromArray(
							[
								A3($mdgriffith$elm_animator$Internal$Timeline$interruptAtExactly, startInterruption, scheduled, last2Events)
							]));
				}
			} else {
				var _v3 = future.a;
				var nextStart = _v3.a;
				var next = _v3.b;
				var nextEvents = _v3.c;
				var futureRemaining = future.b;
				return (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, startInterruption, nextStart) && A2(
					$mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat,
					startInterruption,
					$mdgriffith$elm_animator$Internal$Timeline$startTime(next))) ? $elm$core$Maybe$Just(
					A2(
						$elm$core$List$cons,
						A3($mdgriffith$elm_animator$Internal$Timeline$Line, nextStart, next, nextEvents),
						A2(
							$elm$core$List$cons,
							A3(
								$mdgriffith$elm_animator$Internal$Timeline$interruptAtExactly,
								startInterruption,
								scheduled,
								A4(
									$mdgriffith$elm_animator$Internal$Timeline$LastTwoEvents,
									$mdgriffith$elm_animator$Internal$Timeline$endTime(startEvent),
									$mdgriffith$elm_animator$Internal$Timeline$getEvent(startEvent),
									$mdgriffith$elm_animator$Internal$Timeline$startTime(next),
									$mdgriffith$elm_animator$Internal$Timeline$getEvent(next))),
							futureRemaining))) : $elm$core$Maybe$Nothing;
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$lineStartTime = function (_v0) {
	var start = _v0.a;
	return start;
};
var $mdgriffith$elm_animator$Internal$Timeline$interruptionHappensLater = F2(
	function (startInterruption, remaining) {
		if (!remaining.b) {
			return false;
		} else {
			var top = remaining.a;
			return A2(
				$mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat,
				startInterruption,
				$mdgriffith$elm_animator$Internal$Timeline$lineStartTime(top));
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$interruptLines = F5(
	function (now, startInterruption, scheduled, pastLines, lines) {
		interruptLines:
		while (true) {
			if (!lines.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var startLine = lines.a;
				var remaining = lines.b;
				if (A2($mdgriffith$elm_animator$Internal$Timeline$interruptionHappensLater, startInterruption, remaining)) {
					var $temp$now = now,
						$temp$startInterruption = startInterruption,
						$temp$scheduled = scheduled,
						$temp$pastLines = A2($elm$core$List$cons, startLine, pastLines),
						$temp$lines = remaining;
					now = $temp$now;
					startInterruption = $temp$startInterruption;
					scheduled = $temp$scheduled;
					pastLines = $temp$pastLines;
					lines = $temp$lines;
					continue interruptLines;
				} else {
					var _v1 = A4($mdgriffith$elm_animator$Internal$Timeline$interruptLine, startInterruption, scheduled, startLine, remaining);
					if (_v1.$ === 1) {
						var $temp$now = now,
							$temp$startInterruption = startInterruption,
							$temp$scheduled = scheduled,
							$temp$pastLines = A2($elm$core$List$cons, startLine, pastLines),
							$temp$lines = remaining;
						now = $temp$now;
						startInterruption = $temp$startInterruption;
						scheduled = $temp$scheduled;
						pastLines = $temp$pastLines;
						lines = $temp$lines;
						continue interruptLines;
					} else {
						var interruption = _v1.a;
						return (_Utils_eq(
							startInterruption,
							$mdgriffith$elm_animator$Internal$Timeline$lineStartTime(startLine)) && A2($mdgriffith$elm_animator$Internal$Time$thisAfterThat, startInterruption, now)) ? $elm$core$Maybe$Just(
							_Utils_ap(
								$elm$core$List$reverse(pastLines),
								interruption)) : $elm$core$Maybe$Just(
							_Utils_ap(
								$elm$core$List$reverse(pastLines),
								A2($elm$core$List$cons, startLine, interruption)));
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$interrupt = F3(
	function (details, startAt, scheduled) {
		var _v0 = details.b5;
		var lines = _v0;
		var _v1 = A5($mdgriffith$elm_animator$Internal$Timeline$interruptLines, details.cu, startAt, scheduled, _List_Nil, lines);
		if (_v1.$ === 1) {
			return A3($mdgriffith$elm_animator$Internal$Timeline$enqueue, details, startAt, scheduled);
		} else {
			var interrupted = _v1.a;
			return interrupted;
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$applyInterruptionHelper = F2(
	function (interrupts, timeline) {
		applyInterruptionHelper:
		while (true) {
			if (!interrupts.b) {
				return timeline;
			} else {
				var inter = interrupts.a;
				var remaining = interrupts.b;
				var delay = function () {
					var d = inter.a;
					return d;
				}();
				var newEvents = A3(
					$mdgriffith$elm_animator$Internal$Timeline$interrupt,
					timeline,
					A2($mdgriffith$elm_animator$Internal$Time$advanceBy, delay, timeline.cu),
					inter);
				var $temp$interrupts = remaining,
					$temp$timeline = _Utils_update(
					timeline,
					{b5: newEvents});
				interrupts = $temp$interrupts;
				timeline = $temp$timeline;
				continue applyInterruptionHelper;
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$applyInterruptions = function (timeline) {
	var _v0 = timeline.aQ;
	if (!_v0.b) {
		return timeline;
	} else {
		return A2(
			$mdgriffith$elm_animator$Internal$Timeline$applyInterruptionHelper,
			$elm$core$List$reverse(timeline.aQ),
			_Utils_update(
				timeline,
				{aQ: _List_Nil}));
	}
};
var $mdgriffith$elm_animator$Internal$Timeline$applyQueued = function (timeline) {
	var _v0 = timeline.aU;
	if (_v0.$ === 1) {
		return timeline;
	} else {
		var queued = _v0.a;
		return _Utils_update(
			timeline,
			{
				b5: A3($mdgriffith$elm_animator$Internal$Timeline$enqueue, timeline, timeline.cu, queued),
				aU: $elm$core$Maybe$Nothing
			});
	}
};
var $mdgriffith$elm_animator$Internal$Timeline$dwellingAt = F2(
	function (now, event) {
		var eventStartTime = $mdgriffith$elm_animator$Internal$Timeline$startTime(event);
		var eventEndTime = $mdgriffith$elm_animator$Internal$Timeline$endTime(event);
		return A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, now, eventStartTime) && A2($mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat, now, eventEndTime);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_animator$Internal$Timeline$Captured = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_animator$Internal$Timeline$NothingCaptured = {$: 1};
var $mdgriffith$elm_animator$Internal$Timeline$hewLine = F2(
	function (now, events) {
		hewLine:
		while (true) {
			if (!events.b) {
				return $mdgriffith$elm_animator$Internal$Timeline$NothingCaptured;
			} else {
				var top = events.a;
				var remaining = events.b;
				if (A2($mdgriffith$elm_animator$Internal$Timeline$dwellingAt, now, top)) {
					return $mdgriffith$elm_animator$Internal$Timeline$Captured(
						A3(
							$mdgriffith$elm_animator$Internal$Timeline$Line,
							$mdgriffith$elm_animator$Internal$Timeline$startTime(top),
							top,
							remaining));
				} else {
					if (A2(
						$mdgriffith$elm_animator$Internal$Time$thisAfterThat,
						now,
						$mdgriffith$elm_animator$Internal$Timeline$endTime(top))) {
						var $temp$now = now,
							$temp$events = remaining;
						now = $temp$now;
						events = $temp$events;
						continue hewLine;
					} else {
						return $mdgriffith$elm_animator$Internal$Timeline$NothingCaptured;
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$garbageCollectOldEvents = F3(
	function (now, droppable, lines) {
		garbageCollectOldEvents:
		while (true) {
			if (!lines.b) {
				return $elm$core$List$reverse(droppable);
			} else {
				var _v1 = lines.a;
				var startAt = _v1.a;
				var startingEvent = _v1.b;
				var events = _v1.c;
				var remaining = lines.b;
				if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, startAt, now)) {
					return _Utils_ap(
						$elm$core$List$reverse(droppable),
						lines);
				} else {
					if (A2($mdgriffith$elm_animator$Internal$Timeline$dwellingAt, now, startingEvent)) {
						return lines;
					} else {
						var maybeInterruptionTime = A2(
							$elm$core$Maybe$map,
							$mdgriffith$elm_animator$Internal$Timeline$lineStartTime,
							$elm$core$List$head(remaining));
						var interrupted = function () {
							if (maybeInterruptionTime.$ === 1) {
								return false;
							} else {
								var interruptionTime = maybeInterruptionTime.a;
								return A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, now, interruptionTime);
							}
						}();
						if (interrupted) {
							var $temp$now = now,
								$temp$droppable = A2(
								$elm$core$List$cons,
								A3($mdgriffith$elm_animator$Internal$Timeline$Line, startAt, startingEvent, events),
								droppable),
								$temp$lines = remaining;
							now = $temp$now;
							droppable = $temp$droppable;
							lines = $temp$lines;
							continue garbageCollectOldEvents;
						} else {
							var _v2 = A2($mdgriffith$elm_animator$Internal$Timeline$hewLine, now, events);
							if (_v2.$ === 1) {
								return _Utils_ap(
									$elm$core$List$reverse(droppable),
									lines);
							} else {
								var capturedLine = _v2.a;
								return A2($elm$core$List$cons, capturedLine, remaining);
							}
						}
					}
				}
			}
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$linesAreActive = F2(
	function (now, lines) {
		linesAreActive:
		while (true) {
			if (!lines.b) {
				return false;
			} else {
				var _v1 = lines.a;
				var startAt = _v1.a;
				var startingEvent = _v1.b;
				var events = _v1.c;
				var remaining = lines.b;
				if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, startAt, now)) {
					return true;
				} else {
					var maybeInterruption = function () {
						var _v5 = $elm$core$List$head(remaining);
						if (_v5.$ === 1) {
							return $elm$core$Maybe$Nothing;
						} else {
							var _v6 = _v5.a;
							var interruptionTime = _v6.a;
							return $elm$core$Maybe$Just(interruptionTime);
						}
					}();
					var last = A2(
						$elm$core$Maybe$withDefault,
						startingEvent,
						$elm$core$List$head(
							$elm$core$List$reverse(events)));
					if (!maybeInterruption.$) {
						var interruptTime = maybeInterruption.a;
						if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, interruptTime, now)) {
							return true;
						} else {
							var time = last.b;
							if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, time, now)) {
								return true;
							} else {
								var $temp$now = now,
									$temp$lines = remaining;
								now = $temp$now;
								lines = $temp$lines;
								continue linesAreActive;
							}
						}
					} else {
						var time = last.b;
						if (A2($mdgriffith$elm_animator$Internal$Time$thisAfterOrEqualThat, time, now)) {
							return true;
						} else {
							var $temp$now = now,
								$temp$lines = remaining;
							now = $temp$now;
							lines = $temp$lines;
							continue linesAreActive;
						}
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$clean = F2(
	function (runGC, details) {
		var running = function () {
			var _v1 = details.b5;
			var lines = _v1;
			return A2($mdgriffith$elm_animator$Internal$Timeline$linesAreActive, details.cu, lines);
		}();
		var events = function () {
			var _v0 = details.b5;
			var evs = _v0;
			return evs;
		}();
		return _Utils_update(
			details,
			{
				b5: runGC ? A3($mdgriffith$elm_animator$Internal$Timeline$garbageCollectOldEvents, details.cu, _List_Nil, events) : details.b5,
				bh: running
			});
	});
var $ianmackenzie$elm_units$Quantity$max = F2(
	function (_v0, _v1) {
		var x = _v0;
		var y = _v1;
		return A2($elm$core$Basics$max, x, y);
	});
var $mdgriffith$elm_animator$Internal$Timeline$updateWith = F3(
	function (withGC, possiblyNow, _v0) {
		var timeline = _v0;
		var now = A2(
			$ianmackenzie$elm_units$Quantity$max,
			$mdgriffith$elm_animator$Internal$Time$absolute(possiblyNow),
			timeline.cu);
		return _Utils_eq(timeline.b5, _List_Nil) ? A2(
			$mdgriffith$elm_animator$Internal$Timeline$clean,
			withGC,
			$mdgriffith$elm_animator$Internal$Timeline$applyInterruptions(
				$mdgriffith$elm_animator$Internal$Timeline$applyQueued(
					_Utils_update(
						timeline,
						{
							b5: function () {
								var firstOccurring = A3($mdgriffith$elm_animator$Internal$Timeline$Occurring, timeline.cj, now, now);
								return _List_fromArray(
									[
										A3($mdgriffith$elm_animator$Internal$Timeline$Line, now, firstOccurring, _List_Nil)
									]);
							}(),
							cu: now
						})))) : A2(
			$mdgriffith$elm_animator$Internal$Timeline$clean,
			withGC,
			$mdgriffith$elm_animator$Internal$Timeline$applyInterruptions(
				$mdgriffith$elm_animator$Internal$Timeline$applyQueued(
					_Utils_update(
						timeline,
						{cu: now}))));
	});
var $mdgriffith$elm_animator$Internal$Timeline$update = $mdgriffith$elm_animator$Internal$Timeline$updateWith(true);
var $mdgriffith$elm_animator$Animator$watching = F3(
	function (get, set, _v0) {
		var isRunning = _v0.a;
		var updateModel = _v0.b;
		return A2(
			$mdgriffith$elm_animator$Internal$Timeline$Animator,
			$elm$core$Basics$always(true),
			F2(
				function (now, model) {
					var newModel = A2(updateModel, now, model);
					return A2(
						set,
						A2(
							$mdgriffith$elm_animator$Internal$Timeline$update,
							now,
							get(newModel)),
						newModel);
				}));
	});
var $author$project$Main$animator = A3(
	$mdgriffith$elm_animator$Animator$watching,
	function ($) {
		return $.F;
	},
	F2(
		function (newVisibility, model) {
			return _Utils_update(
				model,
				{F: newVisibility});
		}),
	A3(
		$mdgriffith$elm_animator$Animator$watching,
		function ($) {
			return $.x;
		},
		F2(
			function (newChecked, model) {
				return _Utils_update(
					model,
					{x: newChecked});
			}),
		$mdgriffith$elm_animator$Animator$animator));
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {bD: oldTime, cO: request, c_: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.cO;
		var oldTime = _v0.bD;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 1) {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.c_;
		var oldTime = _v0.bD;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $mdgriffith$elm_animator$Animator$toSubscription = F3(
	function (toMsg, model, _v0) {
		var isRunning = _v0.a;
		return isRunning(model) ? $elm$browser$Browser$Events$onAnimationFrame(toMsg) : $elm$core$Platform$Sub$none;
	});
var $author$project$Screens$Welcome$NoGeoLocationApi = function (a) {
	return {$: 2, a: a};
};
var $author$project$Screens$Welcome$ReceivedLocation = function (a) {
	return {$: 3, a: a};
};
var $author$project$Screens$Welcome$RequestLocationPermsApiError = function (a) {
	return {$: 1, a: a};
};
var $author$project$Ports$errorObtainingCurrentPosition = _Platform_incomingPort('errorObtainingCurrentPosition', $elm$json$Json$Decode$int);
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Ports$locationReceiver = _Platform_incomingPort(
	'locationReceiver',
	A2(
		$elm$json$Json$Decode$andThen,
		function (longitude) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (latitude) {
					return $elm$json$Json$Decode$succeed(
						{bv: latitude, bx: longitude});
				},
				A2($elm$json$Json$Decode$field, 'latitude', $elm$json$Json$Decode$float));
		},
		A2($elm$json$Json$Decode$field, 'longitude', $elm$json$Json$Decode$float)));
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $author$project$Ports$noGeoLocationApiAvailableReceiver = _Platform_incomingPort(
	'noGeoLocationApiAvailableReceiver',
	$elm$json$Json$Decode$null(0));
var $author$project$Screens$Welcome$welcomeScreenSubscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Ports$errorObtainingCurrentPosition($author$project$Screens$Welcome$RequestLocationPermsApiError),
				$author$project$Ports$locationReceiver($author$project$Screens$Welcome$ReceivedLocation),
				$author$project$Ports$noGeoLocationApiAvailableReceiver($author$project$Screens$Welcome$NoGeoLocationApi)
			]));
};
var $author$project$Main$subscriptions = function (model) {
	switch (model.$) {
		case 2:
			var modelData = model.a;
			return A2(
				$elm$core$Platform$Sub$map,
				function (l) {
					return l;
				},
				A3(
					$mdgriffith$elm_animator$Animator$toSubscription,
					function (l) {
						return $author$project$Main$OnMainScreenMsg(
							$author$project$Main$Tick(l));
					},
					modelData,
					$author$project$Main$animator));
		case 1:
			return $elm$core$Platform$Sub$none;
		default:
			var m = model.a;
			return A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$OnWelcomeScreenMsg,
				$author$project$Screens$Welcome$welcomeScreenSubscriptions(m));
	}
};
var $author$project$Main$Error = function (a) {
	return {$: 2, a: a};
};
var $author$project$Utils$Failure = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$GotCurrentZoneLoadingScreenMsg = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$GotWeatherResponse = function (a) {
	return {$: 1, a: a};
};
var $author$project$Utils$Loading = {$: 0};
var $author$project$Main$LoadingScreen = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$LoadingScreenHasCurrentZone = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$LoadingScreenHasNoCurrentZone = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$OnLoadingScreenMsg = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$Refetching = {$: 1};
var $mdgriffith$elm_animator$Animator$TransitionTo = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_animator$Animator$event = $mdgriffith$elm_animator$Animator$TransitionTo;
var $mdgriffith$elm_animator$Animator$initializeSchedule = F2(
	function (waiting, steps) {
		initializeSchedule:
		while (true) {
			if (!steps.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				if (!steps.a.$) {
					var additionalWait = steps.a.a;
					var moreSteps = steps.b;
					var $temp$waiting = A2($ianmackenzie$elm_units$Quantity$plus, waiting, additionalWait),
						$temp$steps = moreSteps;
					waiting = $temp$waiting;
					steps = $temp$steps;
					continue initializeSchedule;
				} else {
					var _v1 = steps.a;
					var dur = _v1.a;
					var checkpoint = _v1.b;
					var moreSteps = steps.b;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							A3(
								$mdgriffith$elm_animator$Internal$Timeline$Schedule,
								waiting,
								A3($mdgriffith$elm_animator$Internal$Timeline$Event, dur, checkpoint, $elm$core$Maybe$Nothing),
								_List_Nil),
							moreSteps));
				}
			}
		}
	});
var $ianmackenzie$elm_units$Duration$seconds = function (numSeconds) {
	return numSeconds;
};
var $ianmackenzie$elm_units$Duration$milliseconds = function (numMilliseconds) {
	return $ianmackenzie$elm_units$Duration$seconds(0.001 * numMilliseconds);
};
var $mdgriffith$elm_animator$Animator$millis = $ianmackenzie$elm_units$Duration$milliseconds;
var $mdgriffith$elm_animator$Internal$Timeline$addToDwell = F2(
	function (duration, maybeDwell) {
		if (!$ianmackenzie$elm_units$Duration$inMilliseconds(duration)) {
			return maybeDwell;
		} else {
			if (maybeDwell.$ === 1) {
				return $elm$core$Maybe$Just(duration);
			} else {
				var existing = maybeDwell.a;
				return $elm$core$Maybe$Just(
					A2($ianmackenzie$elm_units$Quantity$plus, duration, existing));
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$extendEventDwell = F2(
	function (extendBy, thisEvent) {
		var at = thisEvent.a;
		var ev = thisEvent.b;
		var maybeDwell = thisEvent.c;
		return (!$ianmackenzie$elm_units$Duration$inMilliseconds(extendBy)) ? thisEvent : A3(
			$mdgriffith$elm_animator$Internal$Timeline$Event,
			at,
			ev,
			A2($mdgriffith$elm_animator$Internal$Timeline$addToDwell, extendBy, maybeDwell));
	});
var $mdgriffith$elm_animator$Animator$stepsToEvents = F2(
	function (currentStep, _v0) {
		var delay = _v0.a;
		var startEvent = _v0.b;
		var events = _v0.c;
		if (!events.b) {
			if (!currentStep.$) {
				var waiting = currentStep.a;
				return A3(
					$mdgriffith$elm_animator$Internal$Timeline$Schedule,
					delay,
					A2($mdgriffith$elm_animator$Internal$Timeline$extendEventDwell, waiting, startEvent),
					events);
			} else {
				var dur = currentStep.a;
				var checkpoint = currentStep.b;
				return A3(
					$mdgriffith$elm_animator$Internal$Timeline$Schedule,
					delay,
					startEvent,
					_List_fromArray(
						[
							A3($mdgriffith$elm_animator$Internal$Timeline$Event, dur, checkpoint, $elm$core$Maybe$Nothing)
						]));
			}
		} else {
			var _v3 = events.a;
			var durationTo = _v3.a;
			var recentEvent = _v3.b;
			var maybeDwell = _v3.c;
			var remaining = events.b;
			if (!currentStep.$) {
				var dur = currentStep.a;
				return A3(
					$mdgriffith$elm_animator$Internal$Timeline$Schedule,
					delay,
					startEvent,
					A2(
						$elm$core$List$cons,
						A3(
							$mdgriffith$elm_animator$Internal$Timeline$Event,
							durationTo,
							recentEvent,
							A2($mdgriffith$elm_animator$Internal$Timeline$addToDwell, dur, maybeDwell)),
						remaining));
			} else {
				var dur = currentStep.a;
				var checkpoint = currentStep.b;
				return _Utils_eq(checkpoint, recentEvent) ? A3(
					$mdgriffith$elm_animator$Internal$Timeline$Schedule,
					delay,
					startEvent,
					A2(
						$elm$core$List$cons,
						A3(
							$mdgriffith$elm_animator$Internal$Timeline$Event,
							durationTo,
							recentEvent,
							A2($mdgriffith$elm_animator$Internal$Timeline$addToDwell, dur, maybeDwell)),
						remaining)) : A3(
					$mdgriffith$elm_animator$Internal$Timeline$Schedule,
					delay,
					startEvent,
					A2(
						$elm$core$List$cons,
						A3($mdgriffith$elm_animator$Internal$Timeline$Event, dur, checkpoint, $elm$core$Maybe$Nothing),
						events));
			}
		}
	});
var $mdgriffith$elm_animator$Animator$interrupt = F2(
	function (steps, _v0) {
		var tl = _v0;
		return _Utils_update(
			tl,
			{
				aQ: function () {
					var _v1 = A2(
						$mdgriffith$elm_animator$Animator$initializeSchedule,
						$mdgriffith$elm_animator$Animator$millis(0),
						steps);
					if (_v1.$ === 1) {
						return tl.aQ;
					} else {
						var _v2 = _v1.a;
						var schedule = _v2.a;
						var otherSteps = _v2.b;
						return A2(
							$elm$core$List$cons,
							A3($elm$core$List$foldl, $mdgriffith$elm_animator$Animator$stepsToEvents, schedule, otherSteps),
							tl.aQ);
					}
				}(),
				bh: true
			});
	});
var $mdgriffith$elm_animator$Animator$go = F3(
	function (duration, ev, timeline) {
		return A2(
			$mdgriffith$elm_animator$Animator$interrupt,
			_List_fromArray(
				[
					A2($mdgriffith$elm_animator$Animator$event, duration, ev)
				]),
			timeline);
	});
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$here = _Time_here(0);
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {em: index, ex: match, eG: number, fc: submatches};
	});
var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
var $elm$core$String$fromList = _String_fromList;
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{dR: false, eB: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $fredcy$elm_parseint$ParseInt$InvalidRadix = function (a) {
	return {$: 2, a: a};
};
var $fredcy$elm_parseint$ParseInt$InvalidChar = function (a) {
	return {$: 0, a: a};
};
var $fredcy$elm_parseint$ParseInt$OutOfRange = function (a) {
	return {$: 1, a: a};
};
var $fredcy$elm_parseint$ParseInt$charOffset = F2(
	function (basis, c) {
		return $elm$core$Char$toCode(c) - $elm$core$Char$toCode(basis);
	});
var $fredcy$elm_parseint$ParseInt$isBetween = F3(
	function (lower, upper, c) {
		var ci = $elm$core$Char$toCode(c);
		return (_Utils_cmp(
			$elm$core$Char$toCode(lower),
			ci) < 1) && (_Utils_cmp(
			ci,
			$elm$core$Char$toCode(upper)) < 1);
	});
var $fredcy$elm_parseint$ParseInt$intFromChar = F2(
	function (radix, c) {
		var validInt = function (i) {
			return (_Utils_cmp(i, radix) < 0) ? $elm$core$Result$Ok(i) : $elm$core$Result$Err(
				$fredcy$elm_parseint$ParseInt$OutOfRange(c));
		};
		var toInt = A3($fredcy$elm_parseint$ParseInt$isBetween, '0', '9', c) ? $elm$core$Result$Ok(
			A2($fredcy$elm_parseint$ParseInt$charOffset, '0', c)) : (A3($fredcy$elm_parseint$ParseInt$isBetween, 'a', 'z', c) ? $elm$core$Result$Ok(
			10 + A2($fredcy$elm_parseint$ParseInt$charOffset, 'a', c)) : (A3($fredcy$elm_parseint$ParseInt$isBetween, 'A', 'Z', c) ? $elm$core$Result$Ok(
			10 + A2($fredcy$elm_parseint$ParseInt$charOffset, 'A', c)) : $elm$core$Result$Err(
			$fredcy$elm_parseint$ParseInt$InvalidChar(c))));
		return A2($elm$core$Result$andThen, validInt, toInt);
	});
var $fredcy$elm_parseint$ParseInt$parseIntR = F2(
	function (radix, rstring) {
		var _v0 = $elm$core$String$uncons(rstring);
		if (_v0.$ === 1) {
			return $elm$core$Result$Ok(0);
		} else {
			var _v1 = _v0.a;
			var c = _v1.a;
			var rest = _v1.b;
			return A2(
				$elm$core$Result$andThen,
				function (ci) {
					return A2(
						$elm$core$Result$andThen,
						function (ri) {
							return $elm$core$Result$Ok(ci + (ri * radix));
						},
						A2($fredcy$elm_parseint$ParseInt$parseIntR, radix, rest));
				},
				A2($fredcy$elm_parseint$ParseInt$intFromChar, radix, c));
		}
	});
var $elm$core$String$reverse = _String_reverse;
var $fredcy$elm_parseint$ParseInt$parseIntRadix = F2(
	function (radix, string) {
		return ((2 <= radix) && (radix <= 36)) ? A2(
			$fredcy$elm_parseint$ParseInt$parseIntR,
			radix,
			$elm$core$String$reverse(string)) : $elm$core$Result$Err(
			$fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var $fredcy$elm_parseint$ParseInt$parseIntHex = $fredcy$elm_parseint$ParseInt$parseIntRadix(16);
var $mdgriffith$elm_ui$Element$rgba255 = F4(
	function (red, green, blue, a) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, a);
	});
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$Main$roundToPlaces = F2(
	function (places, number) {
		var multiplier = A2($elm$core$Basics$pow, 10, places);
		return $elm$core$Basics$round(number * multiplier) / multiplier;
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$Main$hexToColor = function () {
	var pattern = '' + ('^' + ('#?' + ('(?:' + ('(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))' + ('|' + ('(?:([a-f\\d])([a-f\\d])([a-f\\d]))' + ('|' + ('(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))' + ('|' + ('(?:([a-f\\d])([a-f\\d])([a-f\\d])([a-f\\d]))' + (')' + '$')))))))))));
	var extend = function (token) {
		var _v6 = $elm$core$String$toList(token);
		if (_v6.b && (!_v6.b.b)) {
			var t = _v6.a;
			return $elm$core$String$fromList(
				_List_fromArray(
					[t, t]));
		} else {
			return token;
		}
	};
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$String$toLower,
		A2(
			$elm$core$Basics$composeR,
			A2(
				$elm$regex$Regex$findAtMost,
				1,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$regex$Regex$never,
					$elm$regex$Regex$fromString(pattern))),
			A2(
				$elm$core$Basics$composeR,
				$elm$core$List$head,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Maybe$map(
						function ($) {
							return $.fc;
						}),
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$map(
							$elm$core$List$filterMap($elm$core$Basics$identity)),
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Result$fromMaybe('Parsing hex regex failed'),
							$elm$core$Result$andThen(
								function (colors) {
									var _v0 = A2(
										$elm$core$List$map,
										A2($elm$core$Basics$composeR, extend, $fredcy$elm_parseint$ParseInt$parseIntHex),
										colors);
									_v0$2:
									while (true) {
										if (((((_v0.b && (!_v0.a.$)) && _v0.b.b) && (!_v0.b.a.$)) && _v0.b.b.b) && (!_v0.b.b.a.$)) {
											if (_v0.b.b.b.b) {
												if ((!_v0.b.b.b.a.$) && (!_v0.b.b.b.b.b)) {
													var r = _v0.a.a;
													var _v1 = _v0.b;
													var g = _v1.a.a;
													var _v2 = _v1.b;
													var b = _v2.a.a;
													var _v3 = _v2.b;
													var a = _v3.a.a;
													return $elm$core$Result$Ok(
														A4(
															$mdgriffith$elm_ui$Element$rgba255,
															r,
															g,
															b,
															A2($author$project$Main$roundToPlaces, 2, a / 255)));
												} else {
													break _v0$2;
												}
											} else {
												var r = _v0.a.a;
												var _v4 = _v0.b;
												var g = _v4.a.a;
												var _v5 = _v4.b;
												var b = _v5.a.a;
												return $elm$core$Result$Ok(
													A4($mdgriffith$elm_ui$Element$rgba255, r, g, b, 1));
											}
										} else {
											break _v0$2;
										}
									}
									return $elm$core$Result$Err('Parsing ints from hex failed');
								})))))));
}();
var $mdgriffith$elm_animator$Animator$immediately = $mdgriffith$elm_animator$Animator$millis(0);
var $mdgriffith$elm_animator$Animator$slowly = $mdgriffith$elm_animator$Animator$millis(400);
var $mdgriffith$elm_animator$Animator$update = F3(
	function (newTime, _v0, model) {
		var updateModel = _v0.b;
		return A2(updateModel, newTime, model);
	});
var $author$project$Utils$PermissionDenied = 0;
var $author$project$Utils$PositionUnavailable = 1;
var $author$project$Utils$Timeout = 2;
var $author$project$Utils$codeToGeoLocationApiError = function (code) {
	switch (code) {
		case 1:
			return 0;
		case 2:
			return 1;
		default:
			return 2;
	}
};
var $author$project$Utils$geoLocationApiErrorToString = function (error) {
	switch (error) {
		case 0:
			return 'Permission to access location information was denied';
		case 1:
			return 'The location information is unavailable';
		default:
			return 'The request to get user location timed out';
	}
};
var $author$project$Utils$noGeoApiAvailableErrStr = 'No Geolocation API available on your device';
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Ports$requestLocationPerms = _Platform_outgoingPort(
	'requestLocationPerms',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Screens$Welcome$welcomeScreenUpdate = F2(
	function (msg, model) {
		var _v0 = model.aC;
		var lat = _v0.a;
		var lon = _v0.b;
		switch (msg.$) {
			case 0:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{d7: $elm$core$Maybe$Nothing}),
					$author$project$Ports$requestLocationPerms(0));
			case 3:
				var coords = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							eP: $elm$core$Maybe$Just(coords)
						}),
					$elm$core$Platform$Cmd$none);
			case 1:
				var err = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							d7: $elm$core$Maybe$Just(
								$author$project$Utils$geoLocationApiErrorToString(
									$author$project$Utils$codeToGeoLocationApiError(err)))
						}),
					$elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							d7: $elm$core$Maybe$Just($author$project$Utils$noGeoApiAvailableErrStr)
						}),
					$elm$core$Platform$Cmd$none);
			case 4:
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							d7: model.d7,
							aC: _Utils_Tuple2(str, lon)
						}),
					$elm$core$Platform$Cmd$none);
			case 5:
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							d7: model.d7,
							aC: _Utils_Tuple2(lat, str)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var _v2 = $elm$core$String$toFloat(lat);
				if (!_v2.$) {
					var latFloat = _v2.a;
					if ((_Utils_cmp(latFloat, -90) < 0) || (latFloat > 90)) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{d7: $elm$core$Maybe$Nothing, ag: 'Latitude must be between -90 and 90'}),
							$elm$core$Platform$Cmd$none);
					} else {
						var _v3 = $elm$core$String$toFloat(lon);
						if (!_v3.$) {
							var lonFloat = _v3.a;
							return ((_Utils_cmp(lonFloat, -180) < 0) || (lonFloat > 180)) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{d7: $elm$core$Maybe$Nothing, ag: 'Longitude must be between -180 and 180'}),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										eP: $elm$core$Maybe$Just(
											{bv: latFloat, bx: lonFloat})
									}),
								$elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{d7: $elm$core$Maybe$Nothing, ag: 'Longitude must be a valid number'}),
								$elm$core$Platform$Cmd$none);
						}
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{d7: $elm$core$Maybe$Nothing, ag: 'Latitude must be a valid number'}),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Main$update = F2(
	function (topMsg, topModel) {
		var _v0 = _Utils_Tuple2(topMsg, topModel);
		_v0$3:
		while (true) {
			switch (_v0.a.$) {
				case 0:
					if (!_v0.b.$) {
						var msg = _v0.a.a;
						var model = _v0.b.a;
						return function (_v1) {
							var a = _v1.a;
							var b = _v1.b;
							var _v2 = a.eP;
							if (!_v2.$) {
								var latitude = _v2.a.bv;
								var longitude = _v2.a.bx;
								return _Utils_Tuple2(
									$author$project$Main$LoadingScreen(
										$author$project$Main$LoadingScreenHasNoCurrentZone(
											{
												f: model.f,
												p: _Utils_Tuple2(latitude, longitude)
											})),
									A2(
										$elm$core$Platform$Cmd$map,
										$author$project$Main$OnLoadingScreenMsg,
										A2($elm$core$Task$perform, $author$project$Main$GotCurrentZoneLoadingScreenMsg, $elm$time$Time$here)));
							} else {
								return _Utils_Tuple2(
									$author$project$Main$WelcomeScreen(a),
									A2($elm$core$Platform$Cmd$map, $author$project$Main$OnWelcomeScreenMsg, b));
							}
						}(
							A2($author$project$Screens$Welcome$welcomeScreenUpdate, msg, model));
					} else {
						break _v0$3;
					}
				case 1:
					if (_v0.b.$ === 1) {
						var msg = _v0.a.a;
						var model = _v0.b.a;
						if (!model.$) {
							var modelData = model.a;
							switch (msg.$) {
								case 2:
									var zone = msg.a;
									return _Utils_Tuple2(
										$author$project$Main$LoadingScreen(
											$author$project$Main$LoadingScreenHasCurrentZone(
												{f: modelData.f, aP: $author$project$Utils$Loading, p: modelData.p, k: zone})),
										A2(
											$elm$core$Platform$Cmd$map,
											$author$project$Main$OnLoadingScreenMsg,
											A4($author$project$Api$getData, modelData.p, modelData.f, zone, $author$project$Main$GotWeatherResponse)));
								case 1:
									return _Utils_Tuple2(
										$author$project$Main$LoadingScreen(
											$author$project$Main$LoadingScreenHasNoCurrentZone(modelData)),
										$elm$core$Platform$Cmd$none);
								default:
									return _Utils_Tuple2(
										$author$project$Main$LoadingScreen(
											$author$project$Main$LoadingScreenHasNoCurrentZone(modelData)),
										$elm$core$Platform$Cmd$none);
							}
						} else {
							var modelData = model.a;
							switch (msg.$) {
								case 1:
									var result = msg.a;
									if (!result.$) {
										var data = result.a;
										return _Utils_Tuple2(
											$author$project$Main$MainScreen(
												{
													B: data,
													dY: '',
													F: $mdgriffith$elm_animator$Animator$init(false),
													x: $mdgriffith$elm_animator$Animator$init($author$project$Main$NotRefetching),
													ab: $author$project$Main$NotRefetching,
													f: modelData.f,
													aB: false,
													p: modelData.p,
													t: $author$project$Utils$primary,
													e5: '',
													k: modelData.k
												}),
											function () {
												var _v7 = modelData.p;
												var latitude = _v7.a;
												var longitude = _v7.b;
												return A2(
													$elm$core$Platform$Cmd$map,
													$author$project$Main$OnMainScreenMsg,
													A2(
														$author$project$Api$getReverseGeocoding,
														_Utils_Tuple2(latitude, longitude),
														$author$project$Main$GotCountryAndStateMainScreen));
											}());
									} else {
										var err = result.a;
										return _Utils_Tuple2(
											$author$project$Main$LoadingScreen(
												$author$project$Main$LoadingScreenHasCurrentZone(
													_Utils_update(
														modelData,
														{
															aP: $author$project$Utils$Failure(err)
														}))),
											$elm$core$Platform$Cmd$none);
									}
								case 0:
									return _Utils_Tuple2(
										$author$project$Main$LoadingScreen(
											$author$project$Main$LoadingScreenHasCurrentZone(
												_Utils_update(
													modelData,
													{aP: $author$project$Utils$Loading}))),
										A2(
											$elm$core$Platform$Cmd$map,
											$author$project$Main$OnLoadingScreenMsg,
											A4($author$project$Api$getData, modelData.p, modelData.f, modelData.k, $author$project$Main$GotWeatherResponse)));
								default:
									return _Utils_Tuple2(
										$author$project$Main$LoadingScreen(
											$author$project$Main$LoadingScreenHasCurrentZone(modelData)),
										$elm$core$Platform$Cmd$none);
							}
						}
					} else {
						break _v0$3;
					}
				default:
					if (_v0.b.$ === 2) {
						var msg = _v0.a.a;
						var model = _v0.b.a;
						switch (msg.$) {
							case 5:
								var newTime = msg.a;
								return function (_v9) {
									var a = _v9.a;
									var b = _v9.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									_Utils_Tuple2(
										A3($mdgriffith$elm_animator$Animator$update, newTime, $author$project$Main$animator, model),
										$elm$core$Platform$Cmd$none));
							case 0:
								var hexColor = msg.a;
								return function (_v10) {
									var a = _v10.a;
									var b = _v10.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									_Utils_Tuple2(
										_Utils_update(
											model,
											{
												t: A2(
													$elm$core$Result$withDefault,
													model.t,
													$author$project$Main$hexToColor(hexColor))
											}),
										$elm$core$Platform$Cmd$none));
							case 1:
								return function (_v11) {
									var a = _v11.a;
									var b = _v11.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									_Utils_Tuple2(
										_Utils_update(
											model,
											{aB: true}),
										$elm$core$Platform$Cmd$none));
							case 2:
								return function (_v12) {
									var a = _v12.a;
									var b = _v12.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									_Utils_Tuple2(
										_Utils_update(
											model,
											{aB: false}),
										$elm$core$Platform$Cmd$none));
							case 8:
								var countryAndState = msg.a;
								if (!countryAndState.$) {
									var address = countryAndState.a.dm;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(
											_Utils_update(
												model,
												{
													dY: address.dY,
													F: A3($mdgriffith$elm_animator$Animator$go, $mdgriffith$elm_animator$Animator$slowly, true, model.F),
													e5: address.e5
												})),
										$elm$core$Platform$Cmd$none);
								} else {
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(
											_Utils_update(
												model,
												{
													dY: '',
													F: A3($mdgriffith$elm_animator$Animator$go, $mdgriffith$elm_animator$Animator$slowly, false, model.F),
													e5: ''
												})),
										$elm$core$Platform$Cmd$none);
								}
							case 3:
								return function (_v15) {
									var a = _v15.a;
									var b = _v15.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									_Utils_Tuple2(
										_Utils_update(
											model,
											{
												x: A3($mdgriffith$elm_animator$Animator$go, $mdgriffith$elm_animator$Animator$immediately, $author$project$Main$Refetching, model.x),
												ab: $author$project$Main$Refetching
											}),
										$elm$core$Platform$Cmd$batch(
											_List_fromArray(
												[
													function () {
													var _v14 = model.p;
													var latitude = _v14.a;
													var longitude = _v14.b;
													return A2(
														$author$project$Api$getReverseGeocoding,
														_Utils_Tuple2(latitude, longitude),
														$author$project$Main$GotCountryAndStateMainScreen);
												}(),
													A4($author$project$Api$getData, model.p, model.f, model.k, $author$project$Main$GotRefetchingWeatherResp)
												]))));
							case 6:
								var zone = msg.a;
								return _Utils_Tuple2(
									$author$project$Main$MainScreen(
										_Utils_update(
											model,
											{k: zone})),
									$elm$core$Platform$Cmd$none);
							case 7:
								var time = msg.a;
								return _Utils_Tuple2(
									$author$project$Main$MainScreen(
										_Utils_update(
											model,
											{f: time})),
									$elm$core$Platform$Cmd$none);
							default:
								var result = msg.a;
								return function (_v17) {
									var a = _v17.a;
									var b = _v17.b;
									return _Utils_Tuple2(
										$author$project$Main$MainScreen(a),
										A2($elm$core$Platform$Cmd$map, $author$project$Main$OnMainScreenMsg, b));
								}(
									function () {
										if (!result.$) {
											var data = result.a;
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{
														B: data,
														x: A3($mdgriffith$elm_animator$Animator$go, $mdgriffith$elm_animator$Animator$immediately, $author$project$Main$NotRefetching, model.x),
														ab: $author$project$Main$NotRefetching
													}),
												$elm$core$Platform$Cmd$none);
										} else {
											var err = result.a;
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{
														x: A3(
															$mdgriffith$elm_animator$Animator$go,
															$mdgriffith$elm_animator$Animator$immediately,
															$author$project$Main$Error(err),
															model.x),
														ab: $author$project$Main$Error(err)
													}),
												$elm$core$Platform$Cmd$none);
										}
									}());
						}
					} else {
						break _v0$3;
					}
			}
		}
		var model = _v0.b;
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$ChangedPrimaryColor = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$CloseOptionsMenu = {$: 2};
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $author$project$Utils$black = A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$classes = {dl: 'a', bj: 'atv', $7: 'ab', dp: 'cx', dq: 'cy', dr: 'acb', ds: 'accx', dt: 'accy', du: 'acr', bW: 'al', bX: 'ar', dv: 'at', bl: 'ah', bm: 'av', dz: 's', dF: 'bh', dG: 'b', dJ: 'w7', dL: 'bd', dM: 'bdt', a1: 'bn', dN: 'bs', a2: 'cpe', dT: 'cp', dU: 'cpx', dV: 'cpy', S: 'c', a4: 'ctr', a5: 'cb', a6: 'ccx', T: 'ccy', aM: 'cl', a7: 'cr', dW: 'ct', dZ: 'cptr', d_: 'ctxt', ea: 'fcs', b9: 'focus-within', eb: 'fs', ed: 'g', br: 'hbh', bs: 'hc', ce: 'he', bt: 'hf', cf: 'hfp', eh: 'hv', ej: 'ic', el: 'fr', ba: 'lbl', eo: 'iml', ep: 'imlf', eq: 'imlp', er: 'implw', es: 'it', ev: 'i', cq: 'lnk', aD: 'nb', ct: 'notxt', eH: 'ol', eI: 'or', ai: 'oq', eM: 'oh', cz: 'pg', cA: 'p', eN: 'ppe', eT: 'ui', cR: 'r', eW: 'sb', eX: 'sbx', eY: 'sby', eZ: 'sbt', e0: 'e', e1: 'cap', e2: 'sev', fa: 'sk', c1: 't', fg: 'tc', fh: 'w8', fi: 'w2', fj: 'w9', fk: 'tj', bi: 'tja', fl: 'tl', fm: 'w3', fn: 'w5', fo: 'w4', fp: 'tr', fq: 'w6', fr: 'w1', fs: 'tun', fy: 'ts', an: 'clr', fz: 'u', bS: 'wc', df: 'we', bT: 'wf', dg: 'wfp', bU: 'wrp'};
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$fontWeight = $mdgriffith$elm_ui$Internal$Flag$flag(13);
var $mdgriffith$elm_ui$Element$Font$bold = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.dJ);
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 8};
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var $mdgriffith$elm_ui$Internal$Model$asEl = 2;
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.S);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ed);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cz);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cA);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cR);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.e0);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var $mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eU)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.K)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e3.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e3.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.cR) + ('-' + ($elm$core$String$fromInt(pos.b1) + ('-' + ($elm$core$String$fromInt(pos.de) + ('-' + $elm$core$String$fromInt(pos.cd)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.cl ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.b.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.au) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.cY) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.av))
				])));
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.b9) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.dK),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.dD),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										au: shadow.au,
										av: shadow.av,
										cl: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										cY: shadow.cY
									}));
						},
						focus.e$),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ':focus .focusable, ') + (($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + '.focusable:focus, ') + ('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ' .focusable-thumb'))),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.dK),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.dD),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										au: shadow.au,
										av: shadow.av,
										cl: false,
										b: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.b)),
										cY: shadow.cY
									}));
						},
						focus.e$),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $mdgriffith$elm_ui$Internal$Style$AllChildren = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = 3;
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = 2;
var $mdgriffith$elm_ui$Internal$Style$Self = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var $mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var $mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var $mdgriffith$elm_ui$Internal$Style$Top = 0;
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dW);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a5);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a7);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aM);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a6);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.T);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _v1 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dv);
		case 1:
			var _v2 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.$7);
		case 2:
			var _v3 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bX);
		case 3:
			var _v4 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bW);
		case 4:
			var _v5 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dp);
		default:
			var _v6 = desc;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dq);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.br),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dF),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eZ),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c1),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bs),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dg),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = 0;
var $mdgriffith$elm_ui$Internal$Style$Behind = 5;
var $mdgriffith$elm_ui$Internal$Style$Below = 1;
var $mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var $mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var $mdgriffith$elm_ui$Internal$Style$Within = 4;
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _v0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ej))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-height', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eT),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.el),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aD),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.aD),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dl),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dG),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eI),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eH),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.el),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dF),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ct),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dZ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.d_),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a2),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.an),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ai),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.eh, $mdgriffith$elm_ui$Internal$Style$classes.an)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.eh, $mdgriffith$elm_ui$Internal$Style$classes.ai)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.ea, $mdgriffith$elm_ui$Internal$Style$classes.an)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.ea, $mdgriffith$elm_ui$Internal$Style$classes.ai)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.bj, $mdgriffith$elm_ui$Internal$Style$classes.an)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.bj, $mdgriffith$elm_ui$Internal$Style$classes.ai)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fy),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eW),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eX),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cR),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dT),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dU),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dV),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a1),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dL),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dM),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dN),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c1),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.es),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'inherit')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cR),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.df),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cf),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.du,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ds,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dp),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ds,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dp),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.ds,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.ds + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.du + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.ds)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e2),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ba),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'baseline')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.S),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0px'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', 'min-content'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ce),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bt),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bT),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dg),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bS),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.dr,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.dt,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.dt,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.dt,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dq),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.dt + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.dr + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.dt)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e2),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ed),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.dz + ($mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.dz))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.dz + ($mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.dz))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eo),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.er),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eq),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ep),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cA),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-wrap', 'break-word'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.br),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dF),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c1),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cA),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::after',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::before',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.e0),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.df),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.el),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dF),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dl),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dG),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eI),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.eH),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.c1),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cR),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ed),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fr),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fi),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fm),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fo),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fn),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fq),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dJ),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fh),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fj),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.ev),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fa),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fz),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fz),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fa)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fs),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fk),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bi),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fg),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fp),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.fl),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.dz + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.dz + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cR) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cR) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.a4) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = $elm$core$Basics$identity;
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {a3: closing, q: _List_Nil, X: _List_Nil, I: selector};
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								X: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.X)
							});
					case 3:
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									{a3: '\n}', q: _List_Nil, X: props, I: '@supports (' + (prop + (':' + (value + (') {' + parent.I))))},
									rendered.q)
							});
					case 5:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.I + (' + ' + selector), ''),
										adjRules),
									rendered.q)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.I + (' > ' + child), ''),
										childRules),
									rendered.q)
							});
					case 2:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.I + (' ' + child), ''),
										childRules),
									rendered.q)
							});
					case 4:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.I, descriptor),
											''),
										descriptorRules),
									rendered.q)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								q: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.I, ''),
										batched),
									rendered.q)
							});
				}
			});
		return A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.X;
		if (!_v2.b) {
			return '';
		} else {
			return rule.I + ('{' + (renderValues(rule.X) + (rule.a3 + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.q)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.ez;
	switch (_v0) {
		case 0:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 1:
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.eC;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.c6);
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 1) {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo) {
				case 1:
					var _v2 = options.eh;
					switch (_v2) {
						case 0:
							return _List_Nil;
						case 2:
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 0:
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[
							selector + ('-fs:focus {' + (renderedProps + '\n}')),
							('.' + ($mdgriffith$elm_ui$Internal$Style$classes.dz + (':focus ' + (selector + '-fs  {')))) + (renderedProps + '\n}'),
							(selector + '-fs:focus-within {') + (renderedProps + '\n}'),
							('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.dz) + (' .focusable-thumb' + (selector + '-fs {')))) + (renderedProps + '\n}')
						]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.c6)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 0:
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 13:
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 12:
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 2:
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 1:
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 3:
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 4:
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 5:
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.e0;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cR;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.bU + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bX;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cA;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.cz;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.bW;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.S;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.dz;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 7:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromFloat(top) + ('px ' + ($elm$core$String$fromFloat(right) + ('px ' + ($elm$core$String$fromFloat(bottom) + ('px ' + ($elm$core$String$fromFloat(left) + 'px')))))))
						]));
			case 6:
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 8:
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 0:
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 1:
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 1) {
										if (_v2.b.$ === 1) {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 1) {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 2:
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 1) {
										if (_v7.b.$ === 1) {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 1) {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 3:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.e3.a);
				var ySpacing = toGridLength(template.e3.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.eU)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.K)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.K)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.e3.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.e3.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.K)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eU)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.K)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e3.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e3.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 9:
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.cR) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.cd) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.b1) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.de) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.cR) + (' / ' + ($elm$core$String$fromInt(position.cR + position.cd) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.b1) + (' / ' + ($elm$core$String$fromInt(position.b1 + position.de) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.cR) + ('-' + ($elm$core$String$fromInt(position.b1) + ('-' + ($elm$core$String$fromInt(position.de) + ('-' + $elm$core$String$fromInt(position.cd)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 11:
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((!_v12.a.$) && (!_v12.b.$)) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.c1 + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.c1)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.e1, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.eb, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.e1 + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.e1))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.e1 + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.c1 + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.e1 + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.c1)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {cd: height / size, cY: size, c8: vertical};
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.dQ, adjustment.dE, adjustment.d3, adjustment.ew]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.d3,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.dE,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.dQ,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		dQ: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		cb: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.cd)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.c8) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.cY) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _v2 = _with.dn;
						if (_v2.$ === 1) {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.cb;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.dQ;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 1) {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					bg: _Utils_ap(
						rendered.bg,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					a_: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 1) {
							return rendered.a_;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.a_);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{bg: _List_Nil, a_: _List_Nil},
			stylesheet);
		var topLevel = _v0.a_;
		var rules = _v0.bg;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.ez;
		switch (_v0) {
			case 0:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 1:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ea)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.ea)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$core$Bitwise$and = _Bitwise_and;
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.e0))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.dz, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.T, $mdgriffith$elm_ui$Internal$Style$classes.du])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.dz, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.T, $mdgriffith$elm_ui$Internal$Style$classes.ds])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.dz, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.dt])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.dz, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.a4, $mdgriffith$elm_ui$Internal$Style$classes.dr])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.c1 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bS + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bs)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.dz + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.c1 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.bT + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bt)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.ei, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.fb : _Utils_ap(styled.fb, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.ei, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.fb : _Utils_ap(styled.fb, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.ei, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.fb : _Utils_ap(styled.fb, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.ei, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.fb : _Utils_ap(styled.fb, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.fb : _Utils_ap(rendered.fb, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ae,
						rendered.ah,
						rendered.Z,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.aa)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						ei: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ae,
							rendered.ah,
							rendered.Z,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.aa))),
						fb: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.fb : _Utils_ap(rendered.fb, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ae,
						rendered.ah,
						rendered.Z,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.aa)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						ei: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ae,
							rendered.ah,
							rendered.Z,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.aa))),
						fb: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.dl]));
							case 1:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.dG]));
							case 2:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.eI]));
							case 3:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.eH]));
							case 4:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.el]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.aD, $mdgriffith$elm_ui$Internal$Style$classes.e0, $mdgriffith$elm_ui$Internal$Style$classes.dF]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return $elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.ei, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.bl + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bW);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.bl + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bX);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.bl + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dp);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return $mdgriffith$elm_ui$Internal$Style$classes.bm + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dv);
		case 2:
			return $mdgriffith$elm_ui$Internal$Style$classes.bm + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.$7);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.bm + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.dq);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.ce + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bs,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bt,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.cf + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.dz + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.S + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px !important');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.df + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bS,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.bT,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.dg + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.dz + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.cR + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 1) {
					return {
						Z: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						aa: children,
						ae: has,
						ah: node,
						fb: styles
					};
				} else {
					var _class = _v1.a;
					return {
						Z: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						aa: children,
						ae: has,
						ah: node,
						fb: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.df + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bS),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.bT),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.dg + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.dz + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.cR + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.ce + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bs + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.bt + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.cf + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.dz + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.S + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.fb);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 0};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 4) && (attr.b.$ === 11)) && (!attr.b.a)) {
		var _v1 = attr.b;
		var _v2 = _v1.a;
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onClick = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onClick);
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $mdgriffith$elm_ui$Element$Input$onKeyLookup = function (lookup) {
	var decode = function (code) {
		var _v0 = lookup(code);
		if (_v0.$ === 1) {
			return $elm$json$Json$Decode$fail('No key matched');
		} else {
			var msg = _v0.a;
			return $elm$json$Json$Decode$succeed(msg);
		}
	};
	var isKey = A2(
		$elm$json$Json$Decode$andThen,
		decode,
		A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		A2(
			$elm$html$Html$Events$preventDefaultOn,
			'keydown',
			A2(
				$elm$json$Json$Decode$map,
				function (fired) {
					return _Utils_Tuple2(fired, true);
				},
				isKey)));
};
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.dZ);
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Element$Input$space = ' ';
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.bd;
		var label = _v0.aR;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.a6 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.eZ + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.ct)))))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$pointer,
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											$elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 1) {
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Internal$Model$Attr(
														$elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														$elm$core$List$cons,
														$mdgriffith$elm_ui$Element$Input$onKeyLookup(
															function (code) {
																return _Utils_eq(code, $mdgriffith$elm_ui$Element$Input$enter) ? $elm$core$Maybe$Just(msg) : (_Utils_eq(code, $mdgriffith$elm_ui$Element$Input$space) ? $elm$core$Maybe$Just(msg) : $elm$core$Maybe$Nothing);
															}),
														attrs));
											}
										}()))))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$center = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.fg);
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var $mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var $mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.dW + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.aM)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					attrs)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.eC;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Element$Font$family = function (families) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontFamily,
		A2(
			$mdgriffith$elm_ui$Internal$Model$FontFamily,
			A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'ff-', families),
			families));
};
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $mdgriffith$elm_ui$Element$Font$heavy = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.fj);
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $mdgriffith$elm_ui$Internal$Model$InFront = 4;
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 3) {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 4, element);
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var $mdgriffith$elm_ui$Internal$Model$Layout = 0;
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	dD: $elm$core$Maybe$Nothing,
	dK: $elm$core$Maybe$Nothing,
	e$: $elm$core$Maybe$Just(
		{
			au: 0,
			av: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			b: _Utils_Tuple2(0, 0),
			cY: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _v4 = record.eh;
					if (_v4.$ === 1) {
						return _Utils_update(
							record,
							{
								eh: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _v5 = record.ea;
					if (_v5.$ === 1) {
						return _Utils_update(
							record,
							{
								ea: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.ez;
					if (_v6.$ === 1) {
						return _Utils_update(
							record,
							{
								ez: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			ea: function () {
				var _v0 = record.ea;
				if (_v0.$ === 1) {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			eh: function () {
				var _v1 = record.eh;
				if (_v1.$ === 1) {
					return 1;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			ez: function () {
				var _v2 = record.ez;
				if (_v2.$ === 1) {
					return 0;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{ea: $elm$core$Maybe$Nothing, eh: $elm$core$Maybe$Nothing, ez: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.fb;
				var html = el.a.ei;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.ez;
			if (_v0 === 1) {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.cx;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.eT, $mdgriffith$elm_ui$Internal$Style$classes.dz, $mdgriffith$elm_ui$Internal$Style$classes.e0]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{cx: _List_Nil});
var $mdgriffith$elm_ui$Element$Font$light = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.fm);
var $author$project$Main$RetryFetchingWeather = {$: 0};
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Utils$br = $mdgriffith$elm_ui$Element$html(
	A2($elm$html$Html$br, _List_Nil, _List_Nil));
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX(1);
var $mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Internal$Model$CenterY = 1;
var $mdgriffith$elm_ui$Element$centerY = $mdgriffith$elm_ui$Internal$Model$AlignY(1);
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		if (_Utils_eq(x, y)) {
			var f = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + $elm$core$String$fromInt(x),
					f,
					f,
					f,
					f));
		} else {
			var yFloat = y;
			var xFloat = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
					yFloat,
					xFloat,
					yFloat,
					xFloat));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 9};
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $mdgriffith$elm_ui$Element$paragraph = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asParagraph,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Paragraph),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$spacing(5),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Element$Font$semiBold = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.fq);
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $author$project$Main$initialLoadingScreen = A2(
	$mdgriffith$elm_ui$Element$column,
	_List_fromArray(
		[
			$mdgriffith$elm_ui$Element$centerX,
			$mdgriffith$elm_ui$Element$centerY,
			$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
		]),
	_List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Font$center,
					A2($mdgriffith$elm_ui$Element$paddingXY, 32, 0)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$bold,
							$mdgriffith$elm_ui$Element$Font$size(64)
						]),
					$mdgriffith$elm_ui$Element$text('LOADING')),
					$author$project$Utils$br,
					$author$project$Utils$br,
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$semiBold,
							$mdgriffith$elm_ui$Element$Font$size(18)
						]),
					$mdgriffith$elm_ui$Element$text('Did you know: ')),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$size(18)
						]),
					$mdgriffith$elm_ui$Element$text('Lightning strikes the Earth about 100 times per second.\n'))
				]))
		]));
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.am;
	var right = _v0.al;
	var bottom = _v0._;
	var left = _v0.af;
	if (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) {
		var topFloat = top;
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + $elm$core$String$fromInt(top),
				topFloat,
				topFloat,
				topFloat,
				topFloat));
	} else {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				A4($mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
				top,
				right,
				bottom,
				left));
	}
};
var $author$project$Utils$paddingTop = function (n) {
	return $mdgriffith$elm_ui$Element$paddingEach(
		{_: 0, af: 0, al: 0, am: n});
};
var $author$project$Utils$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $author$project$Main$loadingScreenView = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
			]),
		A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary),
					$mdgriffith$elm_ui$Element$paddingEach(
					{_: 16, af: 0, al: 0, am: 15})
				]),
			function () {
				if (!model.$) {
					return $author$project$Main$initialLoadingScreen;
				} else {
					var modelData = model.a;
					var _v1 = modelData.aP;
					if (!_v1.$) {
						return $author$project$Main$initialLoadingScreen;
					} else {
						var err = _v1.a;
						return A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY]),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$Font$center,
											$mdgriffith$elm_ui$Element$Font$size(54),
											$mdgriffith$elm_ui$Element$Font$semiBold
										]),
									_List_fromArray(
										[
											A2(
											$mdgriffith$elm_ui$Element$el,
											_List_fromArray(
												[$mdgriffith$elm_ui$Element$Font$center]),
											$mdgriffith$elm_ui$Element$text('Something went wrong:')),
											$author$project$Utils$br,
											A2(
											$mdgriffith$elm_ui$Element$el,
											_List_fromArray(
												[$mdgriffith$elm_ui$Element$Font$heavy]),
											function () {
												switch (err.$) {
													case 0:
														var url = err.a;
														return $mdgriffith$elm_ui$Element$text('Bad URL: ' + url);
													case 1:
														return $mdgriffith$elm_ui$Element$text('Timeout');
													case 2:
														return $mdgriffith$elm_ui$Element$text('Network Error');
													case 3:
														var response = err.a;
														return $mdgriffith$elm_ui$Element$text(
															'Bad Status: ' + $elm$core$String$fromInt(response));
													default:
														return $mdgriffith$elm_ui$Element$text('Error parsing body');
												}
											}())
										])),
									A2(
									$mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[
											$author$project$Utils$paddingTop(18),
											$mdgriffith$elm_ui$Element$centerX
										]),
									A2(
										$mdgriffith$elm_ui$Element$Input$button,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$centerX,
												$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black),
												$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$white),
												$mdgriffith$elm_ui$Element$Font$bold,
												A2($mdgriffith$elm_ui$Element$paddingXY, 24, 12),
												$mdgriffith$elm_ui$Element$Font$size(22)
											]),
										{
											aR: $mdgriffith$elm_ui$Element$text('RETRY'),
											bd: $elm$core$Maybe$Just(
												$author$project$Main$OnLoadingScreenMsg($author$project$Main$RetryFetchingWeather))
										}))
								]));
					}
				}
			}()));
};
var $icidasset$elm_material_icons$Material$Icons$Types$Inherit = {$: 1};
var $author$project$Main$OpenOptionsMenu = {$: 1};
var $author$project$Main$RefetchWeatherOnBackground = {$: 3};
var $elm$svg$Svg$Attributes$enableBackground = _VirtualDom_attribute('enable-background');
var $icidasset$elm_material_icons$Material$Icons$Internal$b = $elm$svg$Svg$Attributes$enableBackground;
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $icidasset$elm_material_icons$Material$Icons$Internal$f = $elm$svg$Svg$Attributes$fill;
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $icidasset$elm_material_icons$Material$Icons$Internal$icon = F4(
	function (attributes, nodes, size, coloring) {
		var sizeAsString = $elm$core$String$fromInt(size);
		return A2(
			$elm$svg$Svg$svg,
			_Utils_ap(
				attributes,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$height(sizeAsString),
						$elm$svg$Svg$Attributes$width(sizeAsString)
					])),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							function () {
							if (!coloring.$) {
								var color = coloring.a;
								return $elm$svg$Svg$Attributes$fill(
									$avh4$elm_color$Color$toCssString(color));
							} else {
								return $elm$svg$Svg$Attributes$fill('currentColor');
							}
						}()
						]),
					nodes)
				]));
	});
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $icidasset$elm_material_icons$Material$Icons$Internal$p = $elm$svg$Svg$path;
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $icidasset$elm_material_icons$Material$Icons$Internal$v = $elm$svg$Svg$Attributes$viewBox;
var $icidasset$elm_material_icons$Material$Icons$air = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$b('new 0 0 24 24'),
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$icidasset$elm_material_icons$Material$Icons$Internal$p,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('M0,0h24v24H0V0z'),
							$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
						]),
					_List_Nil)
				])),
			A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$icidasset$elm_material_icons$Material$Icons$Internal$p,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M14.5,17c0,1.65-1.35,3-3,3s-3-1.35-3-3h2c0,0.55,0.45,1,1,1s1-0.45,1-1s-0.45-1-1-1H2v-2h9.5 C13.15,14,14.5,15.35,14.5,17z M19,6.5C19,4.57,17.43,3,15.5,3S12,4.57,12,6.5h2C14,5.67,14.67,5,15.5,5S17,5.67,17,6.5 S16.33,8,15.5,8H2v2h13.5C17.43,10,19,8.43,19,6.5z M18.5,11H2v2h16.5c0.83,0,1.5,0.67,1.5,1.5S19.33,16,18.5,16v2 c1.93,0,3.5-1.57,3.5-3.5S20.43,11,18.5,11z')
								]),
							_List_Nil)
						]))
				]))
		]));
var $mdgriffith$elm_ui$Element$Font$alignRight = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.fp);
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
var $mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			$elm$core$Basics$min,
			1.0,
			A2($elm$core$Basics$max, 0.0, o)));
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + $mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var $mdgriffith$elm_animator$Internal$Interpolate$FullDefault = {$: 0};
var $mdgriffith$elm_animator$Internal$Interpolate$Position = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_animator$Animator$at = $mdgriffith$elm_animator$Internal$Interpolate$Position($mdgriffith$elm_animator$Internal$Interpolate$FullDefault);
var $author$project$Utils$dayToString = function (day) {
	switch (day) {
		case 0:
			return 'Monday';
		case 1:
			return 'Tuesday';
		case 2:
			return 'Wednesday';
		case 3:
			return 'Thursday';
		case 4:
			return 'Friday';
		case 5:
			return 'Saturday';
		default:
			return 'Sunday';
	}
};
var $icidasset$elm_material_icons$Material$Icons$hourglass_empty = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0V0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6 2v6h.01L6 8.01 10 12l-4 4 .01.01H6V22h12v-5.99h-.01L18 16l-4-4 4-3.99-.01-.01H18V2H6zm10 14.5V20H8v-3.5l4-4 4 4zm-4-5l-4-4V4h8v3.5l-4 4z')
				]),
			_List_Nil)
		]));
var $author$project$Main$hourlyDataOfToday = F3(
	function (myZone, currTime, list) {
		var year = A2($elm$time$Time$toYear, myZone, currTime);
		var month = $author$project$Utils$monthToInt(
			A2($elm$time$Time$toMonth, myZone, currTime));
		var day = A2($elm$time$Time$toDay, myZone, currTime);
		return A2(
			$elm$core$List$filter,
			function (hourly) {
				var hourlyYear = A2($elm$time$Time$toYear, $elm$time$Time$utc, hourly.c2);
				var hourlyMonth = $author$project$Utils$monthToInt(
					A2($elm$time$Time$toMonth, $elm$time$Time$utc, hourly.c2));
				var hourlyDay = A2($elm$time$Time$toDay, $elm$time$Time$utc, hourly.c2);
				return _Utils_eq(year, hourlyYear) && (_Utils_eq(month, hourlyMonth) && _Utils_eq(day, hourlyDay));
			},
			list);
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $mdgriffith$elm_ui$Element$link = F2(
	function (attrs, _v0) {
		var url = _v0.c5;
		var label = _v0.aR;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$NodeName('a'),
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$Attr(
					$elm$html$Html$Attributes$href(url)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Internal$Model$Attr(
						$elm$html$Html$Attributes$rel('noopener noreferrer')),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.a6 + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.cq)))),
								attrs))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_animator$Internal$Timeline$Loop = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_animator$Internal$Interpolate$Oscillate = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $ianmackenzie$elm_units$Quantity$ratio = F2(
	function (_v0, _v1) {
		var x = _v0;
		var y = _v1;
		return x / y;
	});
var $mdgriffith$elm_animator$Internal$Timeline$pauseToBounds = F3(
	function (_v0, activeDuration, totalDur) {
		var dur = _v0.a;
		var val = _v0.b;
		var start = A2($ianmackenzie$elm_units$Quantity$multiplyBy, val, activeDuration);
		return _Utils_Tuple2(
			A2($ianmackenzie$elm_units$Quantity$ratio, start, totalDur),
			A2(
				$ianmackenzie$elm_units$Quantity$ratio,
				A2($ianmackenzie$elm_units$Quantity$plus, start, dur),
				totalDur));
	});
var $mdgriffith$elm_animator$Internal$Timeline$pauseValue = function (_v0) {
	var v = _v0.b;
	return v;
};
var $mdgriffith$elm_animator$Internal$Timeline$prepareOscillator = F3(
	function (activeDuration, pauses, osc) {
		var totalDuration = A3(
			$elm$core$List$foldl,
			F2(
				function (_v5, d) {
					var p = _v5.a;
					return A2($ianmackenzie$elm_units$Quantity$plus, p, d);
				}),
			activeDuration,
			pauses);
		var withPause = F3(
			function (u, a, ps) {
				withPause:
				while (true) {
					if (!ps.b) {
						return osc(a);
					} else {
						if (!ps.b.b) {
							var p = ps.a;
							var _v1 = A3($mdgriffith$elm_animator$Internal$Timeline$pauseToBounds, p, activeDuration, totalDuration);
							var start = _v1.a;
							var end = _v1.b;
							if ((_Utils_cmp(u, start) > -1) && (_Utils_cmp(u, end) < 1)) {
								return $mdgriffith$elm_animator$Internal$Timeline$pauseValue(p);
							} else {
								if (_Utils_cmp(u, end) > 0) {
									var pauseDuration = end - start;
									return osc(a - pauseDuration);
								} else {
									return osc(a);
								}
							}
						} else {
							var p = ps.a;
							var _v2 = ps.b;
							var lookahead = _v2.a;
							var remain = _v2.b;
							var _v3 = A3($mdgriffith$elm_animator$Internal$Timeline$pauseToBounds, p, activeDuration, totalDuration);
							var start = _v3.a;
							var end = _v3.b;
							if ((_Utils_cmp(u, start) > -1) && (_Utils_cmp(u, end) < 1)) {
								return $mdgriffith$elm_animator$Internal$Timeline$pauseValue(p);
							} else {
								if (_Utils_cmp(u, end) > 0) {
									var pauseDuration = end - start;
									var gap = function () {
										var _v4 = A3($mdgriffith$elm_animator$Internal$Timeline$pauseToBounds, lookahead, activeDuration, totalDuration);
										var nextPauseStart = _v4.a;
										var nextPauseEnd = _v4.b;
										return (_Utils_cmp(u, nextPauseStart) > -1) ? (nextPauseStart - end) : 0;
									}();
									var $temp$u = u,
										$temp$a = (a + gap) - pauseDuration,
										$temp$ps = A2($elm$core$List$cons, lookahead, remain);
									u = $temp$u;
									a = $temp$a;
									ps = $temp$ps;
									continue withPause;
								} else {
									return osc(a);
								}
							}
						}
					}
				}
			});
		var fn = function (u) {
			return A3(withPause, u, u, pauses);
		};
		return _Utils_Tuple2(fn, totalDuration);
	});
var $mdgriffith$elm_animator$Animator$loop = F2(
	function (activeDuration, osc) {
		if (osc.$ === 1) {
			var i = osc.a;
			return $mdgriffith$elm_animator$Animator$at(i);
		} else {
			var pauses = osc.a;
			var fn = osc.b;
			var _v1 = A3($mdgriffith$elm_animator$Internal$Timeline$prepareOscillator, activeDuration, pauses, fn);
			var preparedFn = _v1.a;
			var totalDuration = _v1.b;
			return A3(
				$mdgriffith$elm_animator$Internal$Interpolate$Oscillate,
				$mdgriffith$elm_animator$Internal$Interpolate$FullDefault,
				$mdgriffith$elm_animator$Internal$Timeline$Loop(totalDuration),
				preparedFn);
		}
	});
var $mdgriffith$elm_ui$Element$Font$medium = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.fn);
var $icidasset$elm_material_icons$Material$Icons$menu = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 18h18v-2H3v2zm0-5h18v-2H3v2zm0-7v2h18V6H3z')
				]),
			_List_Nil)
		]));
var $mdgriffith$elm_ui$Internal$Model$Monospace = {$: 2};
var $mdgriffith$elm_ui$Element$Font$monospace = $mdgriffith$elm_ui$Internal$Model$Monospace;
var $author$project$Utils$monthToString = function (month) {
	switch (month) {
		case 0:
			return 'January';
		case 1:
			return 'February';
		case 2:
			return 'March';
		case 3:
			return 'April';
		case 4:
			return 'May';
		case 5:
			return 'June';
		case 6:
			return 'July';
		case 7:
			return 'August';
		case 8:
			return 'September';
		case 9:
			return 'October';
		case 10:
			return 'November';
		default:
			return 'December';
	}
};
var $ianmackenzie$elm_units$Quantity$greaterThan = F2(
	function (_v0, _v1) {
		var y = _v0;
		var x = _v1;
		return _Utils_cmp(x, y) > 0;
	});
var $mdgriffith$elm_animator$Internal$Time$inMilliseconds = function (_v0) {
	var ms = _v0;
	return ms;
};
var $mdgriffith$elm_animator$Internal$Time$duration = F2(
	function (one, two) {
		return A2($ianmackenzie$elm_units$Quantity$greaterThan, two, one) ? $ianmackenzie$elm_units$Duration$milliseconds(
			A2(
				$elm$core$Basics$max,
				0,
				$mdgriffith$elm_animator$Internal$Time$inMilliseconds(one) - $mdgriffith$elm_animator$Internal$Time$inMilliseconds(two))) : $ianmackenzie$elm_units$Duration$milliseconds(
			A2(
				$elm$core$Basics$max,
				0,
				$mdgriffith$elm_animator$Internal$Time$inMilliseconds(two) - $mdgriffith$elm_animator$Internal$Time$inMilliseconds(one)));
	});
var $mdgriffith$elm_animator$Internal$Timeline$adjustTime = F4(
	function (lookup, getPersonality, unmodified, upcomingOccurring) {
		var event = unmodified.a;
		var start = unmodified.b;
		var eventEnd = unmodified.c;
		if (!upcomingOccurring.b) {
			return unmodified;
		} else {
			var _v1 = upcomingOccurring.a;
			var next = _v1.a;
			var nextStartTime = _v1.b;
			var personality = getPersonality(
				lookup(event));
			if (!(!personality.d1)) {
				var totalDuration = A2($mdgriffith$elm_animator$Internal$Time$duration, eventEnd, nextStartTime);
				var nextPersonality = getPersonality(
					lookup(next));
				var totalPortions = A2($elm$core$Basics$max, personality.d1 + nextPersonality.dB, 1);
				var lateBy = A2($ianmackenzie$elm_units$Quantity$multiplyBy, personality.d1 / totalPortions, totalDuration);
				return A3(
					$mdgriffith$elm_animator$Internal$Timeline$Occurring,
					event,
					start,
					A2($mdgriffith$elm_animator$Internal$Time$advanceBy, lateBy, eventEnd));
			} else {
				return unmodified;
			}
		}
	});
var $ianmackenzie$elm_units$Quantity$minus = F2(
	function (_v0, _v1) {
		var y = _v0;
		var x = _v1;
		return x - y;
	});
var $mdgriffith$elm_animator$Internal$Time$rollbackBy = F2(
	function (dur, time) {
		return A2(
			$ianmackenzie$elm_units$Quantity$minus,
			$ianmackenzie$elm_units$Duration$inMilliseconds(dur),
			time);
	});
var $mdgriffith$elm_animator$Internal$Time$zeroDuration = function (_v0) {
	var dur = _v0;
	return !dur;
};
var $mdgriffith$elm_animator$Internal$Timeline$adjustTimeWithPrevious = F5(
	function (lookup, getPersonality, _v0, unmodified, upcomingOccurring) {
		var prev = _v0.a;
		var prevStart = _v0.b;
		var prevEnd = _v0.c;
		var event = unmodified.a;
		var start = unmodified.b;
		var eventEnd = unmodified.c;
		var totalPrevDuration = A2($mdgriffith$elm_animator$Internal$Time$duration, prevEnd, start);
		var prevPersonality = getPersonality(
			lookup(prev));
		var personality = getPersonality(
			lookup(event));
		var totalPrevPortions = A2($elm$core$Basics$max, prevPersonality.d1 + personality.dB, 1);
		var earlyBy = A2($ianmackenzie$elm_units$Quantity$multiplyBy, personality.dB / totalPrevPortions, totalPrevDuration);
		if (!upcomingOccurring.b) {
			return $mdgriffith$elm_animator$Internal$Time$zeroDuration(earlyBy) ? unmodified : A3(
				$mdgriffith$elm_animator$Internal$Timeline$Occurring,
				event,
				A2($mdgriffith$elm_animator$Internal$Time$rollbackBy, earlyBy, start),
				eventEnd);
		} else {
			var _v2 = upcomingOccurring.a;
			var next = _v2.a;
			var nextStartTime = _v2.b;
			if (!(!personality.d1)) {
				var totalDuration = A2($mdgriffith$elm_animator$Internal$Time$duration, eventEnd, nextStartTime);
				var nextPersonality = getPersonality(
					lookup(next));
				var totalPortions = A2($elm$core$Basics$max, personality.d1 + nextPersonality.dB, 1);
				var lateBy = A2($ianmackenzie$elm_units$Quantity$multiplyBy, personality.d1 / totalPortions, totalDuration);
				return A3(
					$mdgriffith$elm_animator$Internal$Timeline$Occurring,
					event,
					A2($mdgriffith$elm_animator$Internal$Time$rollbackBy, earlyBy, start),
					A2($mdgriffith$elm_animator$Internal$Time$advanceBy, lateBy, eventEnd));
			} else {
				if ($mdgriffith$elm_animator$Internal$Time$zeroDuration(earlyBy)) {
					return unmodified;
				} else {
					return A3(
						$mdgriffith$elm_animator$Internal$Timeline$Occurring,
						event,
						A2($mdgriffith$elm_animator$Internal$Time$rollbackBy, earlyBy, start),
						eventEnd);
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$hasDwell = function (_v0) {
	var start = _v0.b;
	var end = _v0.c;
	return !(!(start - end));
};
var $mdgriffith$elm_animator$Internal$Timeline$createLookAhead = F4(
	function (fn, lookup, currentEvent, upcomingEvents) {
		if (!upcomingEvents.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var unadjustedUpcoming = upcomingEvents.a;
			var remain = upcomingEvents.b;
			var upcomingOccurring = A5($mdgriffith$elm_animator$Internal$Timeline$adjustTimeWithPrevious, lookup, fn.bk, currentEvent, unadjustedUpcoming, remain);
			return $elm$core$Maybe$Just(
				{
					dx: lookup(
						$mdgriffith$elm_animator$Internal$Timeline$getEvent(upcomingOccurring)),
					eR: $mdgriffith$elm_animator$Internal$Timeline$hasDwell(upcomingOccurring),
					c2: $mdgriffith$elm_animator$Internal$Time$inMilliseconds(
						$mdgriffith$elm_animator$Internal$Timeline$startTime(upcomingOccurring))
				});
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$overLines = F7(
	function (fn, lookup, details, maybePreviousEvent, _v0, futureLines, state) {
		overLines:
		while (true) {
			var lineStart = _v0.a;
			var unadjustedStartEvent = _v0.b;
			var lineRemain = _v0.c;
			var transition = function (newState) {
				if (!futureLines.b) {
					return newState;
				} else {
					var future = futureLines.a;
					var futureStart = future.a;
					var futureStartEv = future.b;
					var futureRemain = future.c;
					var restOfFuture = futureLines.b;
					return A2($mdgriffith$elm_animator$Internal$Time$thisBeforeOrEqualThat, futureStart, details.cu) ? A7($mdgriffith$elm_animator$Internal$Timeline$overLines, fn, lookup, details, $elm$core$Maybe$Nothing, future, restOfFuture, newState) : newState;
				}
			};
			var now = function () {
				if (!futureLines.b) {
					return details.cu;
				} else {
					var _v5 = futureLines.a;
					var futureStart = _v5.a;
					var futureStartEv = _v5.b;
					var futureRemain = _v5.c;
					var restOfFuture = futureLines.b;
					return A2($mdgriffith$elm_animator$Internal$Time$thisBeforeThat, futureStart, details.cu) ? futureStart : details.cu;
				}
			}();
			var lineStartEv = function () {
				if (maybePreviousEvent.$ === 1) {
					return A4($mdgriffith$elm_animator$Internal$Timeline$adjustTime, lookup, fn.bk, unadjustedStartEvent, lineRemain);
				} else {
					var prev = maybePreviousEvent.a;
					return A5($mdgriffith$elm_animator$Internal$Timeline$adjustTimeWithPrevious, lookup, fn.bk, prev, unadjustedStartEvent, lineRemain);
				}
			}();
			if (A2(
				$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
				now,
				$mdgriffith$elm_animator$Internal$Timeline$startTime(lineStartEv))) {
				return transition(
					A7(
						fn.bw,
						$mdgriffith$elm_animator$Internal$Time$inMilliseconds(lineStart),
						$elm$core$Maybe$Just(
							lookup(details.cj)),
						lookup(
							$mdgriffith$elm_animator$Internal$Timeline$getEvent(lineStartEv)),
						$mdgriffith$elm_animator$Internal$Time$inMilliseconds(
							$mdgriffith$elm_animator$Internal$Timeline$startTime(lineStartEv)),
						$mdgriffith$elm_animator$Internal$Time$inMilliseconds(now),
						A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedStartEvent, lineRemain),
						state));
			} else {
				if (A2(
					$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
					now,
					$mdgriffith$elm_animator$Internal$Timeline$endTime(lineStartEv))) {
					return transition(
						A5(
							fn.bQ,
							lookup,
							lineStartEv,
							now,
							A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedStartEvent, lineRemain),
							state));
				} else {
					if (!lineRemain.b) {
						return transition(
							A5(fn.bQ, lookup, lineStartEv, now, $elm$core$Maybe$Nothing, state));
					} else {
						var unadjustedNext = lineRemain.a;
						var lineRemain2 = lineRemain.b;
						var next = A5($mdgriffith$elm_animator$Internal$Timeline$adjustTimeWithPrevious, lookup, fn.bk, unadjustedStartEvent, unadjustedNext, lineRemain2);
						if (A2(
							$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
							now,
							$mdgriffith$elm_animator$Internal$Timeline$startTime(next))) {
							return transition(
								A7(
									fn.bw,
									$mdgriffith$elm_animator$Internal$Time$inMilliseconds(
										$mdgriffith$elm_animator$Internal$Timeline$endTime(lineStartEv)),
									$elm$core$Maybe$Just(
										lookup(
											$mdgriffith$elm_animator$Internal$Timeline$getEvent(lineStartEv))),
									lookup(
										$mdgriffith$elm_animator$Internal$Timeline$getEvent(next)),
									$mdgriffith$elm_animator$Internal$Time$inMilliseconds(
										$mdgriffith$elm_animator$Internal$Timeline$startTime(next)),
									$mdgriffith$elm_animator$Internal$Time$inMilliseconds(now),
									A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext, lineRemain2),
									A5(
										fn.bQ,
										lookup,
										lineStartEv,
										now,
										A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedStartEvent, lineRemain),
										state)));
						} else {
							if (A2(
								$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
								now,
								$mdgriffith$elm_animator$Internal$Timeline$endTime(next))) {
								return transition(
									A5(
										fn.bQ,
										lookup,
										next,
										now,
										A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext, lineRemain2),
										state));
							} else {
								if (!lineRemain2.b) {
									return transition(
										A5(fn.bQ, lookup, next, now, $elm$core$Maybe$Nothing, state));
								} else {
									var unadjustedNext2 = lineRemain2.a;
									var lineRemain3 = lineRemain2.b;
									var next2 = A5($mdgriffith$elm_animator$Internal$Timeline$adjustTimeWithPrevious, lookup, fn.bk, unadjustedNext, unadjustedNext2, lineRemain3);
									if (A2(
										$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
										now,
										$mdgriffith$elm_animator$Internal$Timeline$startTime(next2))) {
										var after = A5(
											fn.bQ,
											lookup,
											next,
											now,
											A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext, lineRemain2),
											state);
										return transition(
											A7(
												fn.bw,
												$mdgriffith$elm_animator$Internal$Time$inMilliseconds(
													$mdgriffith$elm_animator$Internal$Timeline$endTime(next)),
												$elm$core$Maybe$Just(
													lookup(
														$mdgriffith$elm_animator$Internal$Timeline$getEvent(next))),
												lookup(
													$mdgriffith$elm_animator$Internal$Timeline$getEvent(next2)),
												$mdgriffith$elm_animator$Internal$Time$inMilliseconds(
													$mdgriffith$elm_animator$Internal$Timeline$startTime(next2)),
												$mdgriffith$elm_animator$Internal$Time$inMilliseconds(now),
												A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext2, lineRemain3),
												after));
									} else {
										if (A2(
											$mdgriffith$elm_animator$Internal$Time$thisBeforeThat,
											now,
											$mdgriffith$elm_animator$Internal$Timeline$endTime(next2))) {
											return transition(
												A5(
													fn.bQ,
													lookup,
													next2,
													now,
													A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext2, lineRemain3),
													state));
										} else {
											var after = A5(
												fn.bQ,
												lookup,
												next2,
												now,
												A4($mdgriffith$elm_animator$Internal$Timeline$createLookAhead, fn, lookup, unadjustedNext2, lineRemain3),
												state);
											var $temp$fn = fn,
												$temp$lookup = lookup,
												$temp$details = details,
												$temp$maybePreviousEvent = $elm$core$Maybe$Just(next),
												$temp$_v0 = A3(
												$mdgriffith$elm_animator$Internal$Timeline$Line,
												$mdgriffith$elm_animator$Internal$Timeline$endTime(next),
												unadjustedNext2,
												lineRemain3),
												$temp$futureLines = futureLines,
												$temp$state = after;
											fn = $temp$fn;
											lookup = $temp$lookup;
											details = $temp$details;
											maybePreviousEvent = $temp$maybePreviousEvent;
											_v0 = $temp$_v0;
											futureLines = $temp$futureLines;
											state = $temp$state;
											continue overLines;
										}
									}
								}
							}
						}
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Timeline$foldp = F3(
	function (lookup, fn, _v0) {
		var timelineDetails = _v0;
		var _v1 = timelineDetails.b5;
		var timetable = _v1;
		var start = fn.bL(
			lookup(timelineDetails.cj));
		if (!timetable.b) {
			return start;
		} else {
			var firstLine = timetable.a;
			var remainingLines = timetable.b;
			return A7($mdgriffith$elm_animator$Internal$Timeline$overLines, fn, lookup, timelineDetails, $elm$core$Maybe$Nothing, firstLine, remainingLines, start);
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$dwellPeriod = function (movement) {
	if (movement.$ === 1) {
		return $elm$core$Maybe$Nothing;
	} else {
		var period = movement.b;
		return $elm$core$Maybe$Just(period);
	}
};
var $mdgriffith$elm_animator$Internal$Interpolate$getPersonality = function (m) {
	if (!m.$) {
		var personality = m.a;
		return personality;
	} else {
		var personality = m.a;
		return personality;
	}
};
var $ianmackenzie$elm_units$Pixels$inPixels = function (_v0) {
	var numPixels = _v0;
	return numPixels;
};
var $ianmackenzie$elm_units$Pixels$inPixelsPerSecond = function (_v0) {
	var numPixelsPerSecond = _v0;
	return numPixelsPerSecond;
};
var $mdgriffith$elm_animator$Internal$Interpolate$Spline = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_animator$Internal$Interpolate$zeroPoint = {di: 0, dj: 0};
var $mdgriffith$elm_animator$Internal$Interpolate$createSpline = function (config) {
	var totalX = config.b3.di - config.bL.di;
	var startVelScale = 1 / (config.aG.di / totalX);
	var startVelocity = (!config.aO.d2) ? {di: 0, dj: 0} : (((!(config.aG.di - $mdgriffith$elm_animator$Internal$Interpolate$zeroPoint.di)) && (!(config.aG.dj - $mdgriffith$elm_animator$Internal$Interpolate$zeroPoint.dj))) ? {di: totalX * (config.aO.d2 * 3), dj: 0} : {di: (startVelScale * config.aG.di) * (config.aO.d2 * 3), dj: (startVelScale * config.aG.dj) * (config.aO.d2 * 3)});
	var endVelScale = 1 / (config.ay.di / totalX);
	var endVelocity = (!config.aL.dC) ? {di: 0, dj: 0} : (((!(config.ay.di - $mdgriffith$elm_animator$Internal$Interpolate$zeroPoint.di)) && (!(config.ay.dj - $mdgriffith$elm_animator$Internal$Interpolate$zeroPoint.dj))) ? {di: totalX * (config.aL.dC * 3), dj: 0} : {di: (endVelScale * config.ay.di) * (config.aL.dC * 3), dj: (endVelScale * config.ay.dj) * (config.aL.dC * 3)});
	return A4(
		$mdgriffith$elm_animator$Internal$Interpolate$Spline,
		config.bL,
		{di: config.bL.di + ((1 / 3) * startVelocity.di), dj: config.bL.dj + ((1 / 3) * startVelocity.dj)},
		{di: config.b3.di + (((-1) / 3) * endVelocity.di), dj: config.b3.dj + (((-1) / 3) * endVelocity.dj)},
		config.b3);
};
var $mdgriffith$elm_animator$Internal$Interpolate$findAtXOnSpline = F6(
	function (spline, desiredX, tolerance, jumpSize, t, depth) {
		findAtXOnSpline:
		while (true) {
			var p1 = spline.a;
			var p2 = spline.b;
			var p3 = spline.c;
			var p4 = spline.d;
			var point = function () {
				if (t <= 0.5) {
					var q3 = {di: p3.di + (t * (p4.di - p3.di)), dj: p3.dj + (t * (p4.dj - p3.dj))};
					var q2 = {di: p2.di + (t * (p3.di - p2.di)), dj: p2.dj + (t * (p3.dj - p2.dj))};
					var r2 = {di: q2.di + (t * (q3.di - q2.di)), dj: q2.dj + (t * (q3.dj - q2.dj))};
					var q1 = {di: p1.di + (t * (p2.di - p1.di)), dj: p1.dj + (t * (p2.dj - p1.dj))};
					var r1 = {di: q1.di + (t * (q2.di - q1.di)), dj: q1.dj + (t * (q2.dj - q1.dj))};
					return {di: r1.di + (t * (r2.di - r1.di)), dj: r1.dj + (t * (r2.dj - r1.dj))};
				} else {
					var q3 = {di: p4.di + ((1 - t) * (p3.di - p4.di)), dj: p4.dj + ((1 - t) * (p3.dj - p4.dj))};
					var q2 = {di: p3.di + ((1 - t) * (p2.di - p3.di)), dj: p3.dj + ((1 - t) * (p2.dj - p3.dj))};
					var r2 = {di: q3.di + ((1 - t) * (q2.di - q3.di)), dj: q3.dj + ((1 - t) * (q2.dj - q3.dj))};
					var q1 = {di: p2.di + ((1 - t) * (p1.di - p2.di)), dj: p2.dj + ((1 - t) * (p1.dj - p2.dj))};
					var r1 = {di: q2.di + ((1 - t) * (q1.di - q2.di)), dj: q2.dj + ((1 - t) * (q1.dj - q2.dj))};
					return {di: r2.di + ((1 - t) * (r1.di - r2.di)), dj: r2.dj + ((1 - t) * (r1.dj - r2.dj))};
				}
			}();
			if (depth === 10) {
				return {bG: point, bO: t};
			} else {
				if (($elm$core$Basics$abs(point.di - desiredX) < 1) && ($elm$core$Basics$abs(point.di - desiredX) >= 0)) {
					return {bG: point, bO: t};
				} else {
					if ((point.di - desiredX) > 0) {
						var $temp$spline = spline,
							$temp$desiredX = desiredX,
							$temp$tolerance = tolerance,
							$temp$jumpSize = jumpSize / 2,
							$temp$t = t - jumpSize,
							$temp$depth = depth + 1;
						spline = $temp$spline;
						desiredX = $temp$desiredX;
						tolerance = $temp$tolerance;
						jumpSize = $temp$jumpSize;
						t = $temp$t;
						depth = $temp$depth;
						continue findAtXOnSpline;
					} else {
						var $temp$spline = spline,
							$temp$desiredX = desiredX,
							$temp$tolerance = tolerance,
							$temp$jumpSize = jumpSize / 2,
							$temp$t = t + jumpSize,
							$temp$depth = depth + 1;
						spline = $temp$spline;
						desiredX = $temp$desiredX;
						tolerance = $temp$tolerance;
						jumpSize = $temp$jumpSize;
						t = $temp$t;
						depth = $temp$depth;
						continue findAtXOnSpline;
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$interpolateValue = F3(
	function (start, end, t) {
		return (t <= 0.5) ? (start + (t * (end - start))) : (end + ((1 - t) * (start - end)));
	});
var $mdgriffith$elm_animator$Internal$Interpolate$firstDerivativeOnSpline = F2(
	function (_v0, proportion) {
		var p1 = _v0.a;
		var p2 = _v0.b;
		var p3 = _v0.c;
		var p4 = _v0.d;
		var vy3 = p4.dj - p3.dj;
		var vy2 = p3.dj - p2.dj;
		var wy2 = A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, vy2, vy3, proportion);
		var vy1 = p2.dj - p1.dj;
		var wy1 = A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, vy1, vy2, proportion);
		var vx3 = p4.di - p3.di;
		var vx2 = p3.di - p2.di;
		var wx2 = A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, vx2, vx3, proportion);
		var vx1 = p2.di - p1.di;
		var wx1 = A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, vx1, vx2, proportion);
		return {
			di: 3 * A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, wx1, wx2, proportion),
			dj: 3 * A3($mdgriffith$elm_animator$Internal$Interpolate$interpolateValue, wy1, wy2, proportion)
		};
	});
var $mdgriffith$elm_animator$Internal$Interpolate$guessTime = F2(
	function (now, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v0.c;
		var four = _v0.d;
		return (!(four.di - one.di)) ? 0.5 : ((now - one.di) / (four.di - one.di));
	});
var $mdgriffith$elm_animator$Internal$Interpolate$linearDefault = {dB: 0, dC: 0, d1: 0, d2: 0, fD: 0};
var $ianmackenzie$elm_units$Quantity$divideBy = F2(
	function (divisor, _v0) {
		var value = _v0;
		return value / divisor;
	});
var $ianmackenzie$elm_units$Quantity$per = F2(
	function (_v0, _v1) {
		var independentValue = _v0;
		var dependentValue = _v1;
		return dependentValue / independentValue;
	});
var $ianmackenzie$elm_units$Pixels$pixels = function (numPixels) {
	return numPixels;
};
var $mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing = F3(
	function (ease, period, target) {
		var targetPixels = $ianmackenzie$elm_units$Pixels$pixels(
			ease(target));
		var sampleSize = 16;
		var deltaSample = sampleSize / $ianmackenzie$elm_units$Duration$inMilliseconds(period);
		var next = $ianmackenzie$elm_units$Pixels$pixels(
			ease(target + deltaSample));
		var dx2 = A2($ianmackenzie$elm_units$Quantity$minus, targetPixels, next);
		var prev = $ianmackenzie$elm_units$Pixels$pixels(
			ease(target - deltaSample));
		var dx1 = A2($ianmackenzie$elm_units$Quantity$minus, prev, targetPixels);
		var dx = A2(
			$ianmackenzie$elm_units$Quantity$divideBy,
			2,
			A2($ianmackenzie$elm_units$Quantity$plus, dx1, dx2));
		return A2(
			$ianmackenzie$elm_units$Quantity$per,
			$ianmackenzie$elm_units$Duration$milliseconds(sampleSize),
			dx);
	});
var $mdgriffith$elm_animator$Internal$Time$millis = function (ms) {
	return ms;
};
var $ianmackenzie$elm_units$Pixels$pixelsPerSecond = function (numPixelsPerSecond) {
	return numPixelsPerSecond;
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $ianmackenzie$elm_units$Quantity$isInfinite = function (_v0) {
	var value = _v0;
	return $elm$core$Basics$isInfinite(value);
};
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $ianmackenzie$elm_units$Quantity$isNaN = function (_v0) {
	var value = _v0;
	return $elm$core$Basics$isNaN(value);
};
var $ianmackenzie$elm_units$Quantity$zero = 0;
var $mdgriffith$elm_animator$Internal$Interpolate$velocityBetween = F4(
	function (one, oneTime, two, twoTime) {
		var duration = A2($mdgriffith$elm_animator$Internal$Time$duration, oneTime, twoTime);
		var distance = A2($ianmackenzie$elm_units$Quantity$minus, one, two);
		var vel = A2($ianmackenzie$elm_units$Quantity$per, duration, distance);
		return ($ianmackenzie$elm_units$Quantity$isNaN(vel) || $ianmackenzie$elm_units$Quantity$isInfinite(vel)) ? $ianmackenzie$elm_units$Quantity$zero : vel;
	});
var $mdgriffith$elm_animator$Internal$Interpolate$newVelocityAtTarget = F3(
	function (target, targetTime, maybeLookAhead) {
		if (maybeLookAhead.$ === 1) {
			if (target.$ === 1) {
				return $ianmackenzie$elm_units$Pixels$pixelsPerSecond(0);
			} else {
				var period = target.b;
				var toX = target.c;
				if (!period.$) {
					var periodDuration = period.a;
					return A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, 0);
				} else {
					var n = period.a;
					var periodDuration = period.b;
					return A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, 0);
				}
			}
		} else {
			var lookAhead = maybeLookAhead.a;
			var targetPosition = function () {
				if (!target.$) {
					var toX = target.c;
					return $ianmackenzie$elm_units$Pixels$pixels(
						toX(0));
				} else {
					var x = target.b;
					return $ianmackenzie$elm_units$Pixels$pixels(x);
				}
			}();
			var _v3 = lookAhead.dx;
			if (_v3.$ === 1) {
				var aheadPosition = _v3.b;
				return A4(
					$mdgriffith$elm_animator$Internal$Interpolate$velocityBetween,
					targetPosition,
					$mdgriffith$elm_animator$Internal$Time$millis(targetTime),
					$ianmackenzie$elm_units$Pixels$pixels(aheadPosition),
					$mdgriffith$elm_animator$Internal$Time$millis(lookAhead.c2));
			} else {
				var period = _v3.b;
				var toX = _v3.c;
				if (lookAhead.eR) {
					if (!period.$) {
						var periodDuration = period.a;
						return A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, 0);
					} else {
						var n = period.a;
						var periodDuration = period.b;
						return A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, 0);
					}
				} else {
					return A4(
						$mdgriffith$elm_animator$Internal$Interpolate$velocityBetween,
						targetPosition,
						$mdgriffith$elm_animator$Internal$Time$millis(targetTime),
						$ianmackenzie$elm_units$Pixels$pixels(
							toX(0)),
						$mdgriffith$elm_animator$Internal$Time$millis(lookAhead.c2));
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$interpolateBetween = F7(
	function (startTimeInMs, maybePrevious, target, targetTimeInMs, now, maybeLookAhead, state) {
		var targetVelocity = $ianmackenzie$elm_units$Pixels$inPixelsPerSecond(
			A3($mdgriffith$elm_animator$Internal$Interpolate$newVelocityAtTarget, target, targetTimeInMs, maybeLookAhead));
		var targetPosition = function () {
			if (!target.$) {
				var toX = target.c;
				return $ianmackenzie$elm_units$Pixels$pixels(
					toX(0));
			} else {
				var x = target.b;
				return $ianmackenzie$elm_units$Pixels$pixels(x);
			}
		}();
		var curve = $mdgriffith$elm_animator$Internal$Interpolate$createSpline(
			{
				aL: function () {
					if (target.$ === 1) {
						var personality = target.a;
						return personality;
					} else {
						var personality = target.a;
						return personality;
					}
				}(),
				aO: function () {
					if (maybePrevious.$ === 1) {
						return $mdgriffith$elm_animator$Internal$Interpolate$linearDefault;
					} else {
						if (maybePrevious.a.$ === 1) {
							var _v2 = maybePrevious.a;
							var personality = _v2.a;
							return personality;
						} else {
							var _v3 = maybePrevious.a;
							var personality = _v3.a;
							return personality;
						}
					}
				}(),
				b3: {
					di: targetTimeInMs,
					dj: $ianmackenzie$elm_units$Pixels$inPixels(targetPosition)
				},
				ay: {di: 1000, dj: targetVelocity},
				bL: {
					di: startTimeInMs,
					dj: $ianmackenzie$elm_units$Pixels$inPixels(state.O)
				},
				aG: {
					di: 1000,
					dj: $ianmackenzie$elm_units$Pixels$inPixelsPerSecond(state.c7)
				}
			});
		var current = A6(
			$mdgriffith$elm_animator$Internal$Interpolate$findAtXOnSpline,
			curve,
			now,
			1,
			0.25,
			A2($mdgriffith$elm_animator$Internal$Interpolate$guessTime, now, curve),
			0);
		var firstDerivative = A2($mdgriffith$elm_animator$Internal$Interpolate$firstDerivativeOnSpline, curve, current.bO);
		return {
			O: $ianmackenzie$elm_units$Pixels$pixels(current.bG.dj),
			c7: $ianmackenzie$elm_units$Pixels$pixelsPerSecond(1000 * (firstDerivative.dj / firstDerivative.di))
		};
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $mdgriffith$elm_animator$Internal$Spring$criticalDamping = F2(
	function (k, m) {
		return 2 * $elm$core$Basics$sqrt(k * m);
	});
var $elm$core$Basics$e = _Basics_e;
var $mdgriffith$elm_animator$Internal$Spring$toleranceForSpringSettleTimeCalculation = (-1) * A2($elm$core$Basics$logBase, $elm$core$Basics$e, 0.005);
var $mdgriffith$elm_animator$Internal$Spring$settlesAt = function (_v0) {
	var stiffness = _v0.aX;
	var damping = _v0.aN;
	var mass = _v0.aT;
	var m = mass;
	var k = stiffness;
	var springAspect = $elm$core$Basics$sqrt(k / m);
	var cCritical = A2($mdgriffith$elm_animator$Internal$Spring$criticalDamping, k, m);
	var c = damping;
	if (_Utils_eq(
		$elm$core$Basics$round(c),
		$elm$core$Basics$round(cCritical))) {
		return 1000 * (8.5 / springAspect);
	} else {
		if ((c - cCritical) > 0) {
			var dampingAspect = c / cCritical;
			return 1000 * ($mdgriffith$elm_animator$Internal$Spring$toleranceForSpringSettleTimeCalculation / (dampingAspect * springAspect));
		} else {
			var dampingAspect = c / cCritical;
			return 1000 * ($mdgriffith$elm_animator$Internal$Spring$toleranceForSpringSettleTimeCalculation / (dampingAspect * springAspect));
		}
	}
};
var $mdgriffith$elm_animator$Internal$Spring$mapToRange = F3(
	function (minimum, maximum, x) {
		var total = maximum - minimum;
		return minimum + (x * total);
	});
var $mdgriffith$elm_animator$Internal$Spring$wobble2Ratio = F2(
	function (wobble, duration) {
		var ms = $ianmackenzie$elm_units$Duration$inMilliseconds(duration);
		var scalingBelowDur = ms / 350;
		var top = A2(
			$elm$core$Basics$max,
			0.43,
			0.8 * A2($elm$core$Basics$min, 1, scalingBelowDur));
		var bounded = A2(
			$elm$core$Basics$min,
			1,
			A2($elm$core$Basics$max, 0, wobble));
		return A3($mdgriffith$elm_animator$Internal$Spring$mapToRange, 0.43, top, 1 - bounded);
	});
var $mdgriffith$elm_animator$Internal$Spring$wobble2Damping = F4(
	function (wobble, k, m, duration) {
		return A2($mdgriffith$elm_animator$Internal$Spring$wobble2Ratio, wobble, duration) * A2($mdgriffith$elm_animator$Internal$Spring$criticalDamping, k, m);
	});
var $mdgriffith$elm_animator$Internal$Spring$select = F2(
	function (wobbliness, duration) {
		var k = 150;
		var durMS = $ianmackenzie$elm_units$Duration$inMilliseconds(duration);
		var damping = A4($mdgriffith$elm_animator$Internal$Spring$wobble2Damping, wobbliness, k, 1, duration);
		var initiallySettlesAt = $mdgriffith$elm_animator$Internal$Spring$settlesAt(
			{aN: damping, aT: 1, aX: k});
		var newCritical = A2($mdgriffith$elm_animator$Internal$Spring$criticalDamping, k, durMS / initiallySettlesAt);
		return {aN: damping, aT: durMS / initiallySettlesAt, aX: k};
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $mdgriffith$elm_animator$Internal$Spring$step = F4(
	function (target, _v0, dtms, motion) {
		var stiffness = _v0.aX;
		var damping = _v0.aN;
		var mass = _v0.aT;
		var fspring = stiffness * (target - motion.O);
		var fdamper = ((-1) * damping) * motion.c7;
		var dt = dtms / 1000;
		var a = (fspring + fdamper) / mass;
		var newVelocity = motion.c7 + (a * dt);
		var newPos = motion.O + (newVelocity * dt);
		return {O: newPos, c7: newVelocity};
	});
var $mdgriffith$elm_animator$Internal$Spring$stepOver = F4(
	function (duration, params, target, state) {
		var durMS = $ianmackenzie$elm_units$Duration$inMilliseconds(duration);
		var frames = durMS / 16;
		var remainder = 16 * (frames - $elm$core$Basics$floor(frames));
		var steps = (remainder > 0) ? A2(
			$elm$core$List$cons,
			remainder,
			A2(
				$elm$core$List$repeat,
				($elm$core$Basics$floor(durMS) / 16) | 0,
				16)) : A2(
			$elm$core$List$repeat,
			($elm$core$Basics$floor(durMS) / 16) | 0,
			16);
		return A3(
			$elm$core$List$foldl,
			A2($mdgriffith$elm_animator$Internal$Spring$step, target, params),
			state,
			steps);
	});
var $mdgriffith$elm_animator$Internal$Interpolate$springInterpolation = F7(
	function (prevEndTime, _v0, target, targetTime, now, _v1, state) {
		var wobble = function () {
			if (!target.$) {
				var personality = target.a;
				return personality.fD;
			} else {
				var personality = target.a;
				return personality.fD;
			}
		}();
		var targetPos = function () {
			if (!target.$) {
				var toX = target.c;
				return toX(0);
			} else {
				var x = target.b;
				return x;
			}
		}();
		var duration = A2(
			$mdgriffith$elm_animator$Internal$Time$duration,
			$mdgriffith$elm_animator$Internal$Time$millis(prevEndTime),
			$mdgriffith$elm_animator$Internal$Time$millis(targetTime));
		var params = A2($mdgriffith$elm_animator$Internal$Spring$select, wobble, duration);
		var _new = A4(
			$mdgriffith$elm_animator$Internal$Spring$stepOver,
			A2(
				$mdgriffith$elm_animator$Internal$Time$duration,
				$mdgriffith$elm_animator$Internal$Time$millis(prevEndTime),
				$mdgriffith$elm_animator$Internal$Time$millis(now)),
			params,
			targetPos,
			{
				O: $ianmackenzie$elm_units$Pixels$inPixels(state.O),
				c7: $ianmackenzie$elm_units$Pixels$inPixelsPerSecond(state.c7)
			});
		return {
			O: $ianmackenzie$elm_units$Pixels$pixels(_new.O),
			c7: $ianmackenzie$elm_units$Pixels$pixelsPerSecond(_new.c7)
		};
	});
var $mdgriffith$elm_animator$Internal$Interpolate$lerp = F7(
	function (prevEndTime, maybePrev, target, targetTime, now, maybeLookAhead, state) {
		var wobble = function () {
			if (!target.$) {
				var personality = target.a;
				return personality.fD;
			} else {
				var personality = target.a;
				return personality.fD;
			}
		}();
		var nothingHappened = function () {
			if (!target.$) {
				return false;
			} else {
				var x = target.b;
				return _Utils_eq(
					x,
					$ianmackenzie$elm_units$Pixels$inPixels(state.O)) && (!$ianmackenzie$elm_units$Pixels$inPixelsPerSecond(state.c7));
			}
		}();
		if (nothingHappened) {
			return state;
		} else {
			if (maybeLookAhead.$ === 1) {
				return (!(!wobble)) ? A7($mdgriffith$elm_animator$Internal$Interpolate$springInterpolation, prevEndTime, maybePrev, target, targetTime, now, maybeLookAhead, state) : A7($mdgriffith$elm_animator$Internal$Interpolate$interpolateBetween, prevEndTime, maybePrev, target, targetTime, now, maybeLookAhead, state);
			} else {
				return A7($mdgriffith$elm_animator$Internal$Interpolate$interpolateBetween, prevEndTime, maybePrev, target, targetTime, now, maybeLookAhead, state);
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$startMoving = function (movement) {
	return {
		O: function () {
			if (!movement.$) {
				var toX = movement.c;
				return $ianmackenzie$elm_units$Pixels$pixels(
					toX(0));
			} else {
				var x = movement.b;
				return $ianmackenzie$elm_units$Pixels$pixels(x);
			}
		}(),
		c7: $ianmackenzie$elm_units$Pixels$pixelsPerSecond(0)
	};
};
var $mdgriffith$elm_animator$Internal$Time$earliest = F2(
	function (oneQty, twoQty) {
		var one = oneQty;
		var two = twoQty;
		return ((one - two) >= 0) ? twoQty : oneQty;
	});
var $mdgriffith$elm_animator$Internal$Interpolate$wrapUnitAfter = F2(
	function (dur, total) {
		var totalDuration = $elm$core$Basics$round(
			$ianmackenzie$elm_units$Duration$inMilliseconds(total));
		var periodDuration = $elm$core$Basics$round(
			$ianmackenzie$elm_units$Duration$inMilliseconds(dur));
		if ((!periodDuration) || (!totalDuration)) {
			return 0;
		} else {
			var remaining = A2($elm$core$Basics$modBy, periodDuration, totalDuration);
			return (!remaining) ? 1 : (remaining / periodDuration);
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$visit = F5(
	function (lookup, occurring, now, maybeLookAhead, state) {
		var event = occurring.a;
		var start = occurring.b;
		var eventEnd = occurring.c;
		var dwellTime = function () {
			if (maybeLookAhead.$ === 1) {
				return A2($mdgriffith$elm_animator$Internal$Time$duration, start, now);
			} else {
				return A2(
					$mdgriffith$elm_animator$Internal$Time$duration,
					start,
					A2($mdgriffith$elm_animator$Internal$Time$earliest, now, eventEnd));
			}
		}();
		if ($mdgriffith$elm_animator$Internal$Time$zeroDuration(dwellTime)) {
			return {
				O: function () {
					var _v0 = lookup(event);
					if (!_v0.$) {
						var period = _v0.b;
						var toX = _v0.c;
						return $ianmackenzie$elm_units$Pixels$pixels(
							toX(0));
					} else {
						var x = _v0.b;
						return $ianmackenzie$elm_units$Pixels$pixels(x);
					}
				}(),
				c7: A3(
					$mdgriffith$elm_animator$Internal$Interpolate$newVelocityAtTarget,
					lookup(event),
					$mdgriffith$elm_animator$Internal$Time$inMilliseconds(start),
					maybeLookAhead)
			};
		} else {
			var _v1 = lookup(event);
			if (_v1.$ === 1) {
				var pos = _v1.b;
				return {
					O: $ianmackenzie$elm_units$Pixels$pixels(pos),
					c7: $ianmackenzie$elm_units$Pixels$pixelsPerSecond(0)
				};
			} else {
				var period = _v1.b;
				var toX = _v1.c;
				if (!period.$) {
					var periodDuration = period.a;
					var progress = A2($mdgriffith$elm_animator$Internal$Interpolate$wrapUnitAfter, periodDuration, dwellTime);
					return {
						O: $ianmackenzie$elm_units$Pixels$pixels(
							toX(progress)),
						c7: A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, progress)
					};
				} else {
					var n = period.a;
					var periodDuration = period.b;
					var totalMS = $ianmackenzie$elm_units$Duration$inMilliseconds(dwellTime);
					var iterationTimeMS = $ianmackenzie$elm_units$Duration$inMilliseconds(periodDuration);
					var iteration = $elm$core$Basics$floor(totalMS / iterationTimeMS);
					if (_Utils_cmp(iteration, n) > -1) {
						return {
							O: $ianmackenzie$elm_units$Pixels$pixels(
								toX(1)),
							c7: $ianmackenzie$elm_units$Pixels$pixelsPerSecond(0)
						};
					} else {
						var progress = A2($mdgriffith$elm_animator$Internal$Interpolate$wrapUnitAfter, periodDuration, dwellTime);
						return {
							O: $ianmackenzie$elm_units$Pixels$pixels(
								toX(progress)),
							c7: A3($mdgriffith$elm_animator$Internal$Interpolate$derivativeOfEasing, toX, periodDuration, progress)
						};
					}
				}
			}
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$moving = {bk: $mdgriffith$elm_animator$Internal$Interpolate$getPersonality, bp: $mdgriffith$elm_animator$Internal$Interpolate$dwellPeriod, bw: $mdgriffith$elm_animator$Internal$Interpolate$lerp, bL: $mdgriffith$elm_animator$Internal$Interpolate$startMoving, bQ: $mdgriffith$elm_animator$Internal$Interpolate$visit};
var $mdgriffith$elm_animator$Internal$Interpolate$unwrapUnits = function (_v0) {
	var position = _v0.O;
	var velocity = _v0.c7;
	return {
		O: function () {
			var val = position;
			return val;
		}(),
		c7: function () {
			var val = velocity;
			return val;
		}()
	};
};
var $mdgriffith$elm_animator$Internal$Interpolate$details = F2(
	function (timeline, lookup) {
		return $mdgriffith$elm_animator$Internal$Interpolate$unwrapUnits(
			A3($mdgriffith$elm_animator$Internal$Timeline$foldp, lookup, $mdgriffith$elm_animator$Internal$Interpolate$moving, timeline));
	});
var $mdgriffith$elm_animator$Internal$Interpolate$Osc = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_animator$Internal$Interpolate$Pos = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $mdgriffith$elm_animator$Internal$Interpolate$withDefault = F2(
	function (def, defaultOr) {
		if (!defaultOr.$) {
			return def;
		} else {
			var specified = defaultOr.a;
			return specified;
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$fillDefaults = F2(
	function (builtInDefault, specified) {
		if (!specified.$) {
			return builtInDefault;
		} else {
			var partial = specified.a;
			return {
				dB: A2($mdgriffith$elm_animator$Internal$Interpolate$withDefault, builtInDefault.dB, partial.dB),
				dC: A2($mdgriffith$elm_animator$Internal$Interpolate$withDefault, builtInDefault.dC, partial.dC),
				d1: A2($mdgriffith$elm_animator$Internal$Interpolate$withDefault, builtInDefault.d1, partial.d1),
				d2: A2($mdgriffith$elm_animator$Internal$Interpolate$withDefault, builtInDefault.d2, partial.d2),
				fD: A2($mdgriffith$elm_animator$Internal$Interpolate$withDefault, builtInDefault.fD, partial.fD)
			};
		}
	});
var $mdgriffith$elm_animator$Internal$Interpolate$standardDefault = {dB: 0, dC: 0.8, d1: 0, d2: 0.4, fD: 0};
var $mdgriffith$elm_animator$Internal$Interpolate$withStandardDefault = function (defMovement) {
	if (!defMovement.$) {
		var specifiedPersonality = defMovement.a;
		var period = defMovement.b;
		var fn = defMovement.c;
		var personality = A2($mdgriffith$elm_animator$Internal$Interpolate$fillDefaults, $mdgriffith$elm_animator$Internal$Interpolate$standardDefault, specifiedPersonality);
		return A3($mdgriffith$elm_animator$Internal$Interpolate$Osc, personality, period, fn);
	} else {
		var specifiedPersonality = defMovement.a;
		var p = defMovement.b;
		var personality = A2($mdgriffith$elm_animator$Internal$Interpolate$fillDefaults, $mdgriffith$elm_animator$Internal$Interpolate$standardDefault, specifiedPersonality);
		return A2($mdgriffith$elm_animator$Internal$Interpolate$Pos, personality, p);
	}
};
var $mdgriffith$elm_animator$Animator$move = F2(
	function (timeline, lookup) {
		return A2(
			$mdgriffith$elm_animator$Internal$Interpolate$details,
			timeline,
			A2($elm$core$Basics$composeL, $mdgriffith$elm_animator$Internal$Interpolate$withStandardDefault, lookup)).O;
	});
var $mdgriffith$elm_ui$Internal$Model$Empty = {$: 3};
var $mdgriffith$elm_ui$Element$none = $mdgriffith$elm_ui$Internal$Model$Empty;
var $author$project$Main$numberWithSign = function (n) {
	return (n < 0) ? ('-' + $elm$core$String$fromFloat(n)) : ((n > 0) ? ('+' + $elm$core$String$fromFloat(n)) : $elm$core$String$fromFloat(n));
};
var $mdgriffith$elm_ui$Element$padding = function (x) {
	var f = x;
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(x),
			f,
			f,
			f,
			f));
};
var $author$project$Utils$paddingBottom = function (n) {
	return $mdgriffith$elm_ui$Element$paddingEach(
		{_: n, af: 0, al: 0, am: 0});
};
var $elm$core$Basics$pi = _Basics_pi;
var $icidasset$elm_material_icons$Material$Icons$refresh = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M17.65 6.35C16.2 4.9 14.21 4 12 4c-4.42 0-7.99 3.58-7.99 8s3.57 8 7.99 8c3.73 0 6.84-2.55 7.73-6h-2.08c-.82 2.33-3.04 4-5.65 4-3.31 0-6-2.69-6-6s2.69-6 6-6c1.66 0 3.14.69 4.22 1.78L13 11h7V4l-2.35 2.35z')
				]),
			_List_Nil)
		]));
var $mdgriffith$elm_ui$Internal$Model$Rotate = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$rotate = $mdgriffith$elm_ui$Internal$Flag$flag(24);
var $mdgriffith$elm_ui$Element$rotate = function (angle) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$rotate,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Rotate,
			_Utils_Tuple3(0, 0, 1),
			angle));
};
var $mdgriffith$elm_ui$Internal$Flag$borderRound = $mdgriffith$elm_ui$Internal$Flag$flag(17);
var $mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + $elm$core$String$fromInt(radius),
			'border-radius',
			$elm$core$String$fromInt(radius) + 'px'));
};
var $mdgriffith$elm_ui$Internal$Model$AsRow = 0;
var $mdgriffith$elm_ui$Internal$Model$asRow = 0;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.aM + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.T)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$scrollbarX = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.eX);
var $mdgriffith$elm_ui$Element$spaceEvenly = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$spacing, $mdgriffith$elm_ui$Internal$Style$classes.e2);
var $mdgriffith$elm_ui$Element$Font$regular = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontWeight, $mdgriffith$elm_ui$Internal$Style$classes.fo);
var $author$project$Main$statCard = F4(
	function (primaryColor, icon, value, title) {
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(12),
					$mdgriffith$elm_ui$Element$Font$color(primaryColor)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$centerX,
							$author$project$Utils$paddingBottom(8)
						]),
					$mdgriffith$elm_ui$Element$html(
						A2(icon, 52, $icidasset$elm_material_icons$Material$Icons$Types$Inherit))),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$regular,
							$mdgriffith$elm_ui$Element$Font$size(24),
							$mdgriffith$elm_ui$Element$centerX
						]),
					$mdgriffith$elm_ui$Element$text(value)),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$centerX,
							$mdgriffith$elm_ui$Element$Font$size(14),
							$mdgriffith$elm_ui$Element$Font$light
						]),
					$mdgriffith$elm_ui$Element$text(title))
				]));
	});
var $icidasset$elm_material_icons$Material$Icons$sync_problem = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M3 12c0 2.21.91 4.2 2.36 5.64L3 20h6v-6l-2.24 2.24C5.68 15.15 5 13.66 5 12c0-2.61 1.67-4.83 4-5.65V4.26C5.55 5.15 3 8.27 3 12zm8 5h2v-2h-2v2zM21 4h-6v6l2.24-2.24C18.32 8.85 19 10.34 19 12c0 2.61-1.67 4.83-4 5.65v2.09c3.45-.89 6-4.01 6-7.74 0-2.21-.91-4.2-2.36-5.64L21 4zm-10 9h2V7h-2v6z')
				]),
			_List_Nil)
		]));
var $author$project$Main$timeClosestToMine = F4(
	function (zone, time, firstItem, list) {
		var year = A2($elm$time$Time$toYear, zone, time);
		var month = $author$project$Utils$monthToInt(
			A2($elm$time$Time$toMonth, zone, time));
		var minute = A2($elm$time$Time$toMinute, zone, time);
		var hour = A2($elm$time$Time$toHour, zone, time);
		var day = A2($elm$time$Time$toDay, zone, time);
		return A3(
			$elm$core$List$foldl,
			F2(
				function (a, b) {
					var y = A2($elm$time$Time$toYear, $elm$time$Time$utc, a.c2);
					var min = A2($elm$time$Time$toMinute, $elm$time$Time$utc, a.c2);
					var m = $author$project$Utils$monthToInt(
						A2($elm$time$Time$toMonth, $elm$time$Time$utc, a.c2));
					var h = A2($elm$time$Time$toHour, $elm$time$Time$utc, a.c2);
					var d = A2($elm$time$Time$toDay, $elm$time$Time$utc, a.c2);
					return ((_Utils_eq(y, year) && (_Utils_eq(m, month) && (_Utils_eq(d, day) && (_Utils_eq(h, hour) && _Utils_eq(min, minute))))) || ((_Utils_eq(y, year) && (_Utils_eq(m, month) && (_Utils_eq(d, day) && (_Utils_eq(h, hour) && (_Utils_cmp(min, minute) < 0))))) || ((_Utils_eq(y, year) && (_Utils_eq(m, month) && (_Utils_eq(d, day) && (_Utils_cmp(h, hour) < 0)))) || ((_Utils_eq(y, year) && (_Utils_eq(m, month) && (_Utils_cmp(d, day) < 0))) || ((_Utils_eq(y, year) && (_Utils_cmp(m, month) < 0)) || (_Utils_cmp(y, year) < 0)))))) ? a : b;
				}),
			A2(
				$elm$core$Maybe$withDefault,
				firstItem,
				$elm$core$List$head(list)),
			list);
	});
var $author$project$Main$toKm = function (meters) {
	return meters / 1000;
};
var $elm$time$Time$Fri = 4;
var $elm$time$Time$Mon = 0;
var $elm$time$Time$Sat = 5;
var $elm$time$Time$Sun = 6;
var $elm$time$Time$Thu = 3;
var $elm$time$Time$Tue = 1;
var $elm$time$Time$Wed = 2;
var $elm$time$Time$toWeekday = F2(
	function (zone, time) {
		var _v0 = A2(
			$elm$core$Basics$modBy,
			7,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60 * 24));
		switch (_v0) {
			case 0:
				return 3;
			case 1:
				return 4;
			case 2:
				return 5;
			case 3:
				return 6;
			case 4:
				return 0;
			case 5:
				return 1;
			default:
				return 2;
		}
	});
var $icidasset$elm_material_icons$Material$Icons$Outlined$visibility = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0V0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12 6c3.79 0 7.17 2.13 8.82 5.5C19.17 14.87 15.79 17 12 17s-7.17-2.13-8.82-5.5C4.83 8.13 8.21 6 12 6m0-2C7 4 2.73 7.11 1 11.5 2.73 15.89 7 19 12 19s9.27-3.11 11-7.5C21.27 7.11 17 4 12 4zm0 5c1.38 0 2.5 1.12 2.5 2.5S13.38 14 12 14s-2.5-1.12-2.5-2.5S10.62 9 12 9m0-2c-2.48 0-4.5 2.02-4.5 4.5S9.52 16 12 16s4.5-2.02 4.5-4.5S14.48 7 12 7z')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $icidasset$elm_material_icons$Material$Icons$Outlined$water_drop = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$b('new 0 0 24 24'),
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12,2c-5.33,4.55-8,8.48-8,11.8c0,4.98,3.8,8.2,8,8.2s8-3.22,8-8.2C20,10.48,17.33,6.55,12,2z M12,20c-3.35,0-6-2.57-6-6.2 c0-2.34,1.95-5.44,6-9.14c4.05,3.7,6,6.79,6,9.14C18,17.43,15.35,20,12,20z M7.83,14c0.37,0,0.67,0.26,0.74,0.62 c0.41,2.22,2.28,2.98,3.64,2.87c0.43-0.02,0.79,0.32,0.79,0.75c0,0.4-0.32,0.73-0.72,0.75c-2.13,0.13-4.62-1.09-5.19-4.12 C7.01,14.42,7.37,14,7.83,14z')
				]),
			_List_Nil)
		]));
var $mdgriffith$elm_ui$Internal$Flag$borderColor = $mdgriffith$elm_ui$Internal$Flag$flag(28);
var $mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var $author$project$Main$nbsp = '\u00A0';
var $mdgriffith$elm_ui$Internal$Flag$borderStyle = $mdgriffith$elm_ui$Internal$Flag$flag(11);
var $mdgriffith$elm_ui$Element$Border$solid = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$borderStyle, $mdgriffith$elm_ui$Internal$Style$classes.dN);
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + $elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var $author$project$MIcons$icon = F4(
	function (attributes, nodes, size, coloring) {
		return A2(
			$elm$svg$Svg$svg,
			_Utils_ap(
				attributes,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$height(
						$elm$core$String$fromInt(size)),
						$elm$svg$Svg$Attributes$width(
						$elm$core$String$fromInt(size))
					])),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							function () {
							if (!coloring.$) {
								var color = coloring.a;
								return $elm$svg$Svg$Attributes$fill(
									$avh4$elm_color$Color$toCssString(color));
							} else {
								return $elm$svg$Svg$Attributes$fill('currentColor');
							}
						}()
						]),
					nodes)
				]));
	});
var $author$project$MIcons$foggy = A2(
	$author$project$MIcons$icon,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$viewBox('0 96 960 960')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M720 851q-14 0-24.5-10.5T685 816q0-14 10.5-24.5T720 781q14 0 24.5 10.5T755 816q0 14-10.5 24.5T720 851ZM280 976q-14 0-24.5-10.5T245 941q0-14 10.5-24.5T280 906q14 0 24.5 10.5T315 941q0 14-10.5 24.5T280 976Zm-40-125q-14 0-24.5-10.5T205 816q0-14 10.5-24.5T240 781h360q14 0 24.5 10.5T635 816q0 14-10.5 24.5T600 851H240Zm160 125q-14 0-24.5-10.5T365 941q0-14 10.5-24.5T400 906h280q14 0 24.5 10.5T715 941q0 14-10.5 24.5T680 976H400ZM290 716q-86.864 0-148.432-61.52Q80 592.96 80 506.165 80 427 136.5 365 193 303 277 297q32-56 84.5-88.5T480.423 176Q571 176 632.5 233.5T708 376q79 4 125.5 53.5T880 545.623Q880 616 830.417 666 780.833 716 710 716H290Zm0-60h420q46.2 0 78.1-32.5 31.9-32.5 31.9-78T788.1 468q-31.9-32-78.1-32h-60v-30q0-71-49.5-120.5T480.212 236q-51.481 0-93.847 27.5Q344 291 324 338l-8 18h-28q-62 2-105 45.393t-43 104.464Q140 568 183.929 612 227.857 656 290 656Zm190-210Z')
				]),
			_List_Nil)
		]));
var $icidasset$elm_material_icons$Material$Icons$grain = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M10 12c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zM6 8c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm0 8c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm12-8c1.1 0 2-.9 2-2s-.9-2-2-2-2 .9-2 2 .9 2 2 2zm-4 8c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm4-4c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm-4-4c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2zm-4-4c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2z')
				]),
			_List_Nil)
		]));
var $author$project$MIcons$partlyCloudy = A2(
	$author$project$MIcons$icon,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$viewBox('0 96 960 960')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M450 286V136h60v150h-60Zm256 106-42-43 106-106 42 43-106 106Zm64 214v-60h150v60H770Zm1 302L665 802l43-43 106 106-43 43ZM254 392 148 286l42-42 106 106-42 42Zm-14 484h180q33.333 0 56.667-23.265Q500 829.471 500 796.235 500 763 477.237 739 454.475 715 421 715h-44l-18-41q-15.145-35.75-47.6-56.875T240 596q-58.333 0-99.167 40.765-40.833 40.764-40.833 99Q100 794 140.833 835q40.834 41 99.167 41Zm0 60q-83 0-141.5-58.5T40 736q0-83 58.5-141.5T240 536q57.736 0 105.368 32.5T416 655q60.117 0 101.058 43.589Q558 742.178 558 803q-5 56-44.033 94.5Q474.935 936 420 936H240Zm318-133q-3-15.385-6-30t-6-30q52-20 83-65.538 31-45.539 31-101.324Q660 501 607.5 448.5T480 396q-67.215 0-117.627 42.674Q311.962 481.348 303 547q-16-3-31.5-5.5T240 536q14-88 82.5-144T480 336q100 0 170 70t70 170.342Q720 654 675.5 716 631 778 558 803Zm-77-227Z')
				]),
			_List_Nil)
		]));
var $author$project$MIcons$rainy = A2(
	$author$project$MIcons$icon,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$viewBox('0 96 960 960')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M558 973q-11 5-23.5 1T517 959l-69-138q-5-11-1.5-23.5T461 780q11-5 23.5-1t17.5 15l69 138q5 11 1.5 23.5T558 973Zm240-1q-11 5-23.5 1T757 958l-69-138q-5-11-1.5-23.5T701 779q11-5 23.5-1t17.5 15l69 138q5 11 1.5 23.5T798 972Zm-480 0q-11 5-23.5 1.5T277 959l-69-138q-5-11-1-23.5t15-17.5q11-5 23.5-1.5T263 793l69 139q5 11 1 23t-15 17Zm-28-256q-87 0-148.5-61.5T80 506q0-79 56.5-141T277 297q32-56 84.5-88.5T480 176q91 0 152.5 57.5T708 376q79 4 125.5 53.5T880 546q0 70-49.5 120T710 716H290Zm0-60h420q46 0 78-32.5t32-77.5q0-46-32-78t-78-32h-60v-30q0-71-49.5-120.5T480 236q-51 0-93.5 27.5T324 338l-8 18h-28q-62 2-105 45.5T140 506q0 62 44 106t106 44Zm190-210Z')
				]),
			_List_Nil)
		]));
var $author$project$MIcons$snowing = A2(
	$author$project$MIcons$icon,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$viewBox('0 96 960 960')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M230 698q-17 0-28.5-11.5T190 658q0-17 11.5-28.5T230 618q17 0 28.5 11.5T270 658q0 17-11.5 28.5T230 698Zm0-332q-17 0-28.5-11.5T190 326q0-17 11.5-28.5T230 286q17 0 28.5 11.5T270 326q0 17-11.5 28.5T230 366Zm125 500q-17 0-28.5-11.5T315 826q0-17 11.5-28.5T355 786q17 0 28.5 11.5T395 826q0 17-11.5 28.5T355 866Zm0-332q-17 0-28.5-11.5T315 494q0-17 11.5-28.5T355 454q17 0 28.5 11.5T395 494q0 17-11.5 28.5T355 534Zm125 164q-17 0-28.5-11.5T440 658q0-17 11.5-28.5T480 618q17 0 28.5 11.5T520 658q0 17-11.5 28.5T480 698Zm0-332q-17 0-28.5-11.5T440 326q0-17 11.5-28.5T480 286q17 0 28.5 11.5T520 326q0 17-11.5 28.5T480 366Zm125 500q-17 0-28.5-11.5T565 826q0-17 11.5-28.5T605 786q17 0 28.5 11.5T645 826q0 17-11.5 28.5T605 866Zm0-332q-17 0-28.5-11.5T565 494q0-17 11.5-28.5T605 454q17 0 28.5 11.5T645 494q0 17-11.5 28.5T605 534Zm125 164q-17 0-28.5-11.5T690 658q0-17 11.5-28.5T730 618q17 0 28.5 11.5T770 658q0 17-11.5 28.5T730 698Zm0-332q-17 0-28.5-11.5T690 326q0-17 11.5-28.5T730 286q17 0 28.5 11.5T770 326q0 17-11.5 28.5T730 366Z')
				]),
			_List_Nil)
		]));
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $icidasset$elm_material_icons$Material$Icons$thunderstorm = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$b('new 0 0 24 24'),
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
						]),
					_List_Nil)
				])),
			A2(
			$elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$g,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$icidasset$elm_material_icons$Material$Icons$Internal$p,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$d('M17.92,7.02C17.45,4.18,14.97,2,12,2C9.82,2,7.83,3.18,6.78,5.06C4.09,5.41,2,7.74,2,10.5C2,13.53,4.47,16,7.5,16h10 c2.48,0,4.5-2.02,4.5-4.5C22,9.16,20.21,7.23,17.92,7.02z')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$polygon,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$points('14.8,17 11.9,20.32 13.9,21.32 11.55,24 14.2,24 17.1,20.68 15.1,19.68 17.45,17')
								]),
							_List_Nil),
							A2(
							$elm$svg$Svg$polygon,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$points('8.8,17 5.9,20.32 7.9,21.32 5.55,24 8.2,24 11.1,20.68 9.1,19.68 11.45,17')
								]),
							_List_Nil)
						]))
				]))
		]));
var $icidasset$elm_material_icons$Material$Icons$water_drop = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$b('new 0 0 24 24'),
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12,2c-5.33,4.55-8,8.48-8,11.8c0,4.98,3.8,8.2,8,8.2s8-3.22,8-8.2C20,10.48,17.33,6.55,12,2z M7.83,14 c0.37,0,0.67,0.26,0.74,0.62c0.41,2.22,2.28,2.98,3.64,2.87c0.43-0.02,0.79,0.32,0.79,0.75c0,0.4-0.32,0.73-0.72,0.75 c-2.13,0.13-4.62-1.09-5.19-4.12C7.01,14.42,7.37,14,7.83,14z')
				]),
			_List_Nil)
		]));
var $icidasset$elm_material_icons$Material$Icons$wb_cloudy = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M19.36 10.04C18.67 6.59 15.64 4 12 4 9.11 4 6.6 5.64 5.35 8.04 2.34 8.36 0 10.91 0 14c0 3.31 2.69 6 6 6h13c2.76 0 5-2.24 5-5 0-2.64-2.05-4.78-4.64-4.96z')
				]),
			_List_Nil)
		]));
var $icidasset$elm_material_icons$Material$Icons$wb_sunny = A2(
	$icidasset$elm_material_icons$Material$Icons$Internal$icon,
	_List_fromArray(
		[
			$icidasset$elm_material_icons$Material$Icons$Internal$v('0 0 24 24')
		]),
	_List_fromArray(
		[
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M0 0h24v24H0z'),
					$icidasset$elm_material_icons$Material$Icons$Internal$f('none')
				]),
			_List_Nil),
			A2(
			$icidasset$elm_material_icons$Material$Icons$Internal$p,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M6.76 4.84l-1.8-1.79-1.41 1.41 1.79 1.79 1.42-1.41zM4 10.5H1v2h3v-2zm9-9.95h-2V3.5h2V.55zm7.45 3.91l-1.41-1.41-1.79 1.79 1.41 1.41 1.79-1.79zm-3.21 13.7l1.79 1.8 1.41-1.41-1.8-1.79-1.4 1.4zM20 10.5v2h3v-2h-3zm-8-5c-3.31 0-6 2.69-6 6s2.69 6 6 6 6-2.69 6-6-2.69-6-6-6zm-1 16.95h2V19.5h-2v2.95zm-7.45-3.91l1.41 1.41 1.79-1.8-1.41-1.41-1.79 1.8z')
				]),
			_List_Nil)
		]));
var $author$project$MIcons$weatherSnowy = A2(
	$author$project$MIcons$icon,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$viewBox('0 96 960 960')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M259.859 846Q243 846 231.5 834.359t-11.5-28.5Q220 789 231.641 777.5t28.5-11.5Q277 766 288.5 777.641t11.5 28.5Q300 823 288.359 834.5t-28.5 11.5Zm120 130Q363 976 351.5 964.359t-11.5-28.5Q340 919 351.641 907.5t28.5-11.5Q397 896 408.5 907.641t11.5 28.5Q420 953 408.359 964.5t-28.5 11.5Zm120-130Q483 846 471.5 834.359t-11.5-28.5Q460 789 471.641 777.5t28.5-11.5Q517 766 528.5 777.641t11.5 28.5Q540 823 528.359 834.5t-28.5 11.5Zm240 0Q723 846 711.5 834.359t-11.5-28.5Q700 789 711.641 777.5t28.5-11.5Q757 766 768.5 777.641t11.5 28.5Q780 823 768.359 834.5t-28.5 11.5Zm-120 130Q603 976 591.5 964.359t-11.5-28.5Q580 919 591.641 907.5t28.5-11.5Q637 896 648.5 907.641t11.5 28.5Q660 953 648.359 964.5t-28.5 11.5ZM290 676q-86.864 0-148.432-61.52Q80 552.96 80 466.165 80 387 136.5 325 193 263 277 257q32-56 84.5-88.5T480.423 136Q571 136 632.5 193.5T708 336q79 4 125.5 53.5T880 505.623Q880 576 830.417 626 780.833 676 710 676H290Zm0-60h420q46.2 0 78.1-32.5 31.9-32.5 31.9-78T788.1 428q-31.9-32-78.1-32h-60v-30q0-71-49.5-120.5T480.212 196q-51.481 0-93.847 27.5Q344 251 324 298l-8 18h-28q-62 2-105 45.393t-43 104.464Q140 528 183.929 572 227.857 616 290 616Zm190-110Z')
				]),
			_List_Nil)
		]));
var $author$project$Api$wmoCodeToIcon = function (code) {
	switch (code) {
		case 0:
			return $icidasset$elm_material_icons$Material$Icons$wb_sunny;
		case 1:
			return $icidasset$elm_material_icons$Material$Icons$wb_sunny;
		case 2:
			return $author$project$MIcons$partlyCloudy;
		case 3:
			return $icidasset$elm_material_icons$Material$Icons$wb_cloudy;
		case 4:
			return $author$project$MIcons$foggy;
		case 5:
			return $author$project$MIcons$foggy;
		case 6:
			return $icidasset$elm_material_icons$Material$Icons$water_drop;
		case 7:
			return $icidasset$elm_material_icons$Material$Icons$water_drop;
		case 8:
			return $icidasset$elm_material_icons$Material$Icons$water_drop;
		case 9:
			return $author$project$MIcons$snowing;
		case 10:
			return $author$project$MIcons$snowing;
		case 11:
			return $author$project$MIcons$rainy;
		case 12:
			return $author$project$MIcons$rainy;
		case 13:
			return $author$project$MIcons$rainy;
		case 14:
			return $author$project$MIcons$rainy;
		case 15:
			return $author$project$MIcons$rainy;
		case 16:
			return $author$project$MIcons$weatherSnowy;
		case 17:
			return $author$project$MIcons$weatherSnowy;
		case 18:
			return $author$project$MIcons$weatherSnowy;
		case 19:
			return $icidasset$elm_material_icons$Material$Icons$grain;
		case 20:
			return $author$project$MIcons$rainy;
		case 21:
			return $author$project$MIcons$rainy;
		case 22:
			return $author$project$MIcons$rainy;
		case 23:
			return $author$project$MIcons$weatherSnowy;
		case 24:
			return $author$project$MIcons$weatherSnowy;
		case 25:
			return $icidasset$elm_material_icons$Material$Icons$thunderstorm;
		case 26:
			return $icidasset$elm_material_icons$Material$Icons$thunderstorm;
		default:
			return $icidasset$elm_material_icons$Material$Icons$thunderstorm;
	}
};
var $author$project$Main$weeklyForecastCard = F3(
	function (date, max, code) {
		var month = $author$project$Utils$monthToString(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, date));
		var day = $elm$core$String$fromInt(
			A2($elm$time$Time$toDay, $elm$time$Time$utc, date));
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Border$color($author$project$Utils$black),
					$mdgriffith$elm_ui$Element$Border$rounded(14),
					$mdgriffith$elm_ui$Element$Border$solid,
					$mdgriffith$elm_ui$Element$Border$width(3),
					$mdgriffith$elm_ui$Element$padding(14),
					$mdgriffith$elm_ui$Element$Font$bold,
					$mdgriffith$elm_ui$Element$spacing(12)
				]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[$mdgriffith$elm_ui$Element$centerX]),
					$mdgriffith$elm_ui$Element$text(
						$elm$core$String$fromFloat(max) + '°')),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[$mdgriffith$elm_ui$Element$centerX]),
					$mdgriffith$elm_ui$Element$html(
						A3($author$project$Api$wmoCodeToIcon, code, 24, $icidasset$elm_material_icons$Material$Icons$Types$Inherit))),
					A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$centerX,
							$mdgriffith$elm_ui$Element$Font$size(14)
						]),
					$mdgriffith$elm_ui$Element$text(
						_Utils_ap(
							day,
							_Utils_ap(
								$author$project$Main$nbsp,
								A2($elm$core$String$left, 3, month)))))
				]));
	});
var $author$project$Api$wmoCodeToString = function (code) {
	switch (code) {
		case 0:
			return 'Clear sky';
		case 1:
			return 'Mainly clear';
		case 2:
			return 'Partly cloudy';
		case 3:
			return 'Overcast';
		case 4:
			return 'Fog';
		case 5:
			return 'Rime fog';
		case 6:
			return 'Light drizzle';
		case 7:
			return 'Moderate drizzle';
		case 8:
			return 'Dense drizzle';
		case 9:
			return 'Light freezing drizzle';
		case 10:
			return 'Dense freezing drizzle';
		case 11:
			return 'Slight rain';
		case 12:
			return 'Moderate rain';
		case 13:
			return 'Heavy rain';
		case 14:
			return 'Light freezing rain';
		case 15:
			return 'Heavy freezing rain';
		case 16:
			return 'Slight snow fall';
		case 17:
			return 'Moderate snow fall';
		case 18:
			return 'Heavy snow fall';
		case 19:
			return 'Snow grains';
		case 20:
			return 'Slight rain showers';
		case 21:
			return 'Moderate rain showers';
		case 22:
			return 'Violent rain showers';
		case 23:
			return 'Slight snow showers';
		case 24:
			return 'Heavy snow showers';
		case 25:
			return 'Slight or moderate thunderstorm';
		case 26:
			return 'Slight thunderstorm with hail';
		default:
			return 'Heavy thunderstorm with hail';
	}
};
var $mdgriffith$elm_animator$Internal$Timeline$Oscillator = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_animator$Animator$wrap = F2(
	function (start, end) {
		var total = end - start;
		return A2(
			$mdgriffith$elm_animator$Internal$Timeline$Oscillator,
			_List_Nil,
			function (u) {
				return (u === 1) ? start : (start + (total * u));
			});
	});
var $author$project$Main$mainScreen = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
			]),
		A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Background$color(model.t),
					$mdgriffith$elm_ui$Element$paddingEach(
					{_: 16, af: 0, al: 0, am: 15})
				]),
			A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$centerY,
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$author$project$Utils$paddingBottom(15)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$Input$button,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$padding(15),
										$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$black),
										$mdgriffith$elm_ui$Element$Font$heavy,
										$mdgriffith$elm_ui$Element$Font$center
									]),
								{
									aR: A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$rotate(
												A2(
													$mdgriffith$elm_animator$Animator$move,
													model.x,
													function (state) {
														switch (state.$) {
															case 0:
																return $mdgriffith$elm_animator$Animator$at(0);
															case 1:
																return A2(
																	$mdgriffith$elm_animator$Animator$loop,
																	$mdgriffith$elm_animator$Animator$millis(800),
																	A2($mdgriffith$elm_animator$Animator$wrap, 0, 2 * $elm$core$Basics$pi));
															default:
																return $mdgriffith$elm_animator$Animator$at(0);
														}
													}))
											]),
										function () {
											var _v1 = model.ab;
											switch (_v1.$) {
												case 0:
													return $mdgriffith$elm_ui$Element$html(
														A2($icidasset$elm_material_icons$Material$Icons$refresh, 28, $icidasset$elm_material_icons$Material$Icons$Types$Inherit));
												case 1:
													return $mdgriffith$elm_ui$Element$html(
														A2($icidasset$elm_material_icons$Material$Icons$hourglass_empty, 28, $icidasset$elm_material_icons$Material$Icons$Types$Inherit));
												default:
													return $mdgriffith$elm_ui$Element$html(
														A2($icidasset$elm_material_icons$Material$Icons$sync_problem, 28, $icidasset$elm_material_icons$Material$Icons$Types$Inherit));
											}
										}()),
									bd: $elm$core$Maybe$Just($author$project$Main$RefetchWeatherOnBackground)
								}),
								A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
										$mdgriffith$elm_ui$Element$spacing(6),
										$mdgriffith$elm_ui$Element$alpha(
										A2(
											$mdgriffith$elm_animator$Animator$move,
											model.F,
											function (state) {
												return state ? $mdgriffith$elm_animator$Animator$at(1) : $mdgriffith$elm_animator$Animator$at(0);
											}))
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$centerX,
												$mdgriffith$elm_ui$Element$centerY,
												$mdgriffith$elm_ui$Element$Font$heavy,
												$mdgriffith$elm_ui$Element$Font$size(28)
											]),
										$mdgriffith$elm_ui$Element$text(
											(model.e5 === '') ? ((model.dY === '') ? model.dY : '') : model.e5)),
										A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$centerY, $mdgriffith$elm_ui$Element$Font$center]),
										$mdgriffith$elm_ui$Element$text(model.dY))
									])),
								A2(
								$mdgriffith$elm_ui$Element$Input$button,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$padding(15),
										$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$black),
										$mdgriffith$elm_ui$Element$Font$heavy,
										$mdgriffith$elm_ui$Element$Font$center
									]),
								{
									aR: $mdgriffith$elm_ui$Element$html(
										A2($icidasset$elm_material_icons$Material$Icons$menu, 28, $icidasset$elm_material_icons$Material$Icons$Types$Inherit)),
									bd: $elm$core$Maybe$Just($author$project$Main$OpenOptionsMenu)
								})
							])),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$centerX,
								$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black),
								A2($mdgriffith$elm_ui$Element$paddingXY, 16, 8),
								$mdgriffith$elm_ui$Element$Border$rounded(200)
							]),
						A2(
							$mdgriffith$elm_ui$Element$paragraph,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$Font$color(model.t),
									$mdgriffith$elm_ui$Element$Font$size(14),
									$mdgriffith$elm_ui$Element$Font$light
								]),
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$text(
									$author$project$Utils$dayToString(
										A2($elm$time$Time$toWeekday, model.k, model.f))),
									$mdgriffith$elm_ui$Element$text(', '),
									$mdgriffith$elm_ui$Element$text(
									$elm$core$String$fromInt(
										A2($elm$time$Time$toDay, model.k, model.f))),
									$mdgriffith$elm_ui$Element$text(' '),
									$mdgriffith$elm_ui$Element$text(
									$author$project$Utils$monthToString(
										A2($elm$time$Time$toMonth, model.k, model.f)))
								]))),
						function () {
						var _v2 = model.B.eg;
						if (_v2.b) {
							var x = _v2.a;
							var xs = _v2.b;
							var closestHourly = A4($author$project$Main$timeClosestToMine, model.k, model.f, x, xs);
							var actualTemp = function () {
								var _v3 = closestHourly.c0;
								if (!_v3.$) {
									var val = _v3.a;
									return $elm$core$String$fromInt(
										$elm$core$Basics$round(val));
								} else {
									return '--';
								}
							}();
							return A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$Font$center,
												$mdgriffith$elm_ui$Element$Font$bold,
												$mdgriffith$elm_ui$Element$paddingEach(
												{_: 12, af: 0, al: 0, am: 14})
											]),
										$mdgriffith$elm_ui$Element$text(
											$author$project$Api$wmoCodeToString(
												A2($elm$core$Maybe$withDefault, 0, closestHourly.db)))),
										A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$Font$center,
												$mdgriffith$elm_ui$Element$Font$size(182),
												$mdgriffith$elm_ui$Element$Font$medium
											]),
										$mdgriffith$elm_ui$Element$text(actualTemp + '°'))
									]));
						} else {
							return $mdgriffith$elm_ui$Element$text('--');
						}
					}(),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$paddingEach(
								{_: 0, af: 15, al: 0, am: 15}),
								$mdgriffith$elm_ui$Element$Font$heavy
							]),
						$mdgriffith$elm_ui$Element$text('Daily summary')),
						function () {
						var _v4 = model.B.eg;
						if (_v4.b) {
							var x = _v4.a;
							var xs = _v4.b;
							var todayHourlyData = A3($author$project$Main$hourlyDataOfToday, model.k, model.f, model.B.eg);
							var lowestTempOfToday = $author$project$Main$numberWithSign(
								A2(
									$elm$core$Maybe$withDefault,
									0,
									A3(
										$elm$core$List$foldr,
										function (l) {
											if (!l.$) {
												var val = l.a;
												return $elm$core$Maybe$map(
													$elm$core$Basics$min(val));
											} else {
												return $elm$core$Maybe$map(
													$elm$core$Basics$min(0));
											}
										},
										$elm$core$Maybe$Just(999),
										A2(
											$elm$core$List$map,
											function ($) {
												return $.c0;
											},
											todayHourlyData))));
							var highestTempOfToday = $author$project$Main$numberWithSign(
								A2(
									$elm$core$Maybe$withDefault,
									0,
									A3(
										$elm$core$List$foldr,
										function (l) {
											if (!l.$) {
												var val = l.a;
												return $elm$core$Maybe$map(
													$elm$core$Basics$max(val));
											} else {
												return $elm$core$Maybe$map(
													$elm$core$Basics$max(0));
											}
										},
										$elm$core$Maybe$Just(0),
										A2(
											$elm$core$List$map,
											function ($) {
												return $.c0;
											},
											todayHourlyData))));
							var closestHourly = A4($author$project$Main$timeClosestToMine, model.k, model.f, x, xs);
							var perceivedTemp = function () {
								var _v6 = closestHourly.bY;
								if (!_v6.$) {
									var val = _v6.a;
									return $author$project$Main$numberWithSign(val);
								} else {
									return '--';
								}
							}();
							var actualTemp = function () {
								var _v5 = closestHourly.c0;
								if (!_v5.$) {
									var val = _v5.a;
									return $author$project$Main$numberWithSign(val);
								} else {
									return '--';
								}
							}();
							return A2(
								$mdgriffith$elm_ui$Element$paragraph,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$paddingEach(
										{_: 0, af: 15, al: 0, am: 14}),
										$mdgriffith$elm_ui$Element$Font$bold,
										$mdgriffith$elm_ui$Element$Font$size(16),
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$text('Now it feels like ' + (perceivedTemp + ('°, it\'s actually ' + (actualTemp + '°')))),
										$author$project$Utils$br,
										$mdgriffith$elm_ui$Element$text('Today, the temperature is felt in the range from ' + (lowestTempOfToday + ('° to ' + (highestTempOfToday + '°'))))
									]));
						} else {
							return $mdgriffith$elm_ui$Element$none;
						}
					}(),
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$padding(15),
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								A2(
									$mdgriffith$elm_ui$Element$row,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$white),
											$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black),
											$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
											$mdgriffith$elm_ui$Element$padding(30),
											$mdgriffith$elm_ui$Element$Border$rounded(12),
											$mdgriffith$elm_ui$Element$spaceEvenly
										]),
									_List_fromArray(
										[
											A4(
											$author$project$Main$statCard,
											model.t,
											$icidasset$elm_material_icons$Material$Icons$air,
											function () {
												var _v9 = model.B.eg;
												if (_v9.b) {
													var x = _v9.a;
													var xs = _v9.b;
													return $elm$core$String$fromInt(
														$elm$core$Basics$round(
															A2(
																$elm$core$Maybe$withDefault,
																0,
																A4($author$project$Main$timeClosestToMine, model.k, model.f, x, xs).dh))) + 'km/h';
												} else {
													return '--';
												}
											}(),
											'Wind'),
											A4(
											$author$project$Main$statCard,
											model.t,
											$icidasset$elm_material_icons$Material$Icons$Outlined$water_drop,
											function () {
												var _v10 = model.B.eg;
												if (_v10.b) {
													var x = _v10.a;
													var xs = _v10.b;
													return $elm$core$String$fromInt(
														A4($author$project$Main$timeClosestToMine, model.k, model.f, x, xs).cL) + '%';
												} else {
													return '--';
												}
											}(),
											'Humidity'),
											A4(
											$author$project$Main$statCard,
											model.t,
											$icidasset$elm_material_icons$Material$Icons$Outlined$visibility,
											function () {
												var _v11 = model.B.eg;
												if (_v11.b) {
													var x = _v11.a;
													var xs = _v11.b;
													return $elm$core$String$fromInt(
														$elm$core$Basics$round(
															$author$project$Main$toKm(
																A4($author$project$Main$timeClosestToMine, model.k, model.f, x, xs).da))) + 'km';
												} else {
													return '--';
												}
											}(),
											'Visibility')
										]))),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$paddingEach(
										{_: 0, af: 15, al: 0, am: 15}),
										$mdgriffith$elm_ui$Element$Font$heavy
									]),
								$mdgriffith$elm_ui$Element$text('Weekly Forecast')),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								A2(
									$mdgriffith$elm_ui$Element$row,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$padding(15),
											$mdgriffith$elm_ui$Element$spacing(18),
											$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
											$mdgriffith$elm_ui$Element$scrollbarX
										]),
									A2(
										$elm$core$List$map,
										function (_v12) {
											var date = _v12.a;
											var code = _v12.b;
											var max = _v12.c;
											return A3($author$project$Main$weeklyForecastCard, date, max, code);
										},
										model.B.d$))),
								A2(
								$mdgriffith$elm_ui$Element$paragraph,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$Font$alignRight,
										$mdgriffith$elm_ui$Element$paddingEach(
										{_: 0, af: 0, al: 8, am: 8})
									]),
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$text('Weather data by '),
										A2(
										$mdgriffith$elm_ui$Element$link,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$Font$family(
												_List_fromArray(
													[$mdgriffith$elm_ui$Element$Font$monospace])),
												$mdgriffith$elm_ui$Element$Font$color(
												A3($mdgriffith$elm_ui$Element$rgb, 0, 0, 1))
											]),
										{
											aR: $mdgriffith$elm_ui$Element$text('Open-Meteo.com'),
											c5: 'https://open-meteo.com/'
										})
									]))
							]))
					]))));
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $mdgriffith$elm_ui$Internal$Model$map = F2(
	function (fn, el) {
		switch (el.$) {
			case 1:
				var styled = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						ei: F2(
							function (add, context) {
								return A2(
									$elm$virtual_dom$VirtualDom$map,
									fn,
									A2(styled.ei, add, context));
							}),
						fb: styled.fb
					});
			case 0:
				var html = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A2(
						$elm$core$Basics$composeL,
						$elm$virtual_dom$VirtualDom$map(fn),
						html));
			case 2:
				var str = el.a;
				return $mdgriffith$elm_ui$Internal$Model$Text(str);
			default:
				return $mdgriffith$elm_ui$Internal$Model$Empty;
		}
	});
var $mdgriffith$elm_ui$Element$map = $mdgriffith$elm_ui$Internal$Model$map;
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$px = $mdgriffith$elm_ui$Internal$Model$Px;
var $mdgriffith$elm_ui$Element$Font$sansSerif = $mdgriffith$elm_ui$Internal$Model$SansSerif;
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $fredcy$elm_parseint$ParseInt$charFromInt = function (i) {
	return (i < 10) ? $elm$core$Char$fromCode(
		i + $elm$core$Char$toCode('0')) : ((i < 36) ? $elm$core$Char$fromCode(
		(i - 10) + $elm$core$Char$toCode('A')) : '?');
};
var $fredcy$elm_parseint$ParseInt$toRadixUnsafe = F2(
	function (radix, i) {
		return (_Utils_cmp(i, radix) < 0) ? $elm$core$String$fromChar(
			$fredcy$elm_parseint$ParseInt$charFromInt(i)) : _Utils_ap(
			A2($fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, (i / radix) | 0),
			$elm$core$String$fromChar(
				$fredcy$elm_parseint$ParseInt$charFromInt(
					A2($elm$core$Basics$modBy, radix, i))));
	});
var $fredcy$elm_parseint$ParseInt$toRadix = F2(
	function (radix, i) {
		return ((2 <= radix) && (radix <= 36)) ? ((i < 0) ? $elm$core$Result$Ok(
			'-' + A2($fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, -i)) : $elm$core$Result$Ok(
			A2($fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, i))) : $elm$core$Result$Err(
			$fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var $author$project$Main$toHex = function (i) {
	return A3(
		$elm$core$Basics$composeR,
		$elm$core$Result$withDefault(''),
		A2($elm$core$String$padLeft, 2, '0'),
		A2($fredcy$elm_parseint$ParseInt$toRadix, 16, i));
};
var $mdgriffith$elm_ui$Element$toRgb = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	return {aq: a, dH: b, ec: g, eQ: r};
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $mdgriffith$elm_ui$Element$Font$typeface = $mdgriffith$elm_ui$Internal$Model$Typeface;
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Screens$Welcome$OnChangeLatitude = function (a) {
	return {$: 4, a: a};
};
var $author$project$Screens$Welcome$OnChangeLongitude = function (a) {
	return {$: 5, a: a};
};
var $author$project$Screens$Welcome$RequestLocationPerms = {$: 0};
var $author$project$Screens$Welcome$SubmitManualLocationForm = {$: 6};
var $mdgriffith$elm_ui$Element$Input$Above = 2;
var $mdgriffith$elm_ui$Element$Input$Label = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Element$Input$labelAbove = $mdgriffith$elm_ui$Element$Input$Label(2);
var $mdgriffith$elm_ui$Element$Input$Placeholder = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$Input$placeholder = $mdgriffith$elm_ui$Element$Input$Placeholder;
var $mdgriffith$elm_ui$Element$Font$strike = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.fa);
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 0, a: a};
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 1};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 6};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 1) {
			var labelText = label.a;
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asColumn,
				$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position) {
				case 2:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ba),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 3:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ba),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 0:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ba),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ba),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = 5;
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, 5, element);
};
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 1, a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 1) {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 1) {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.dT);
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		$mdgriffith$elm_ui$Element$Border$rounded(3),
		$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$darkGrey),
		$mdgriffith$elm_ui$Element$Background$color($mdgriffith$elm_ui$Element$Input$white),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$spacing(5),
		$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink)
	]);
var $mdgriffith$elm_ui$Element$Input$getHeight = function (attr) {
	if (attr.$ === 8) {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 5, a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 1) {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return true;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isConstrained;
			default:
				var l = len.b;
				return true;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (!label.$) {
		var loc = label.a;
		switch (loc) {
			case 0:
				return false;
			case 1:
				return false;
			case 2:
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {_: -box._, af: -box.af, al: -box.al, am: -box.am};
};
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 2:
				return true;
			case 1:
				return false;
			case 0:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isPixel = function (len) {
	isPixel:
	while (true) {
		switch (len.$) {
			case 1:
				return false;
			case 0:
				return true;
			case 2:
				return false;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
		}
	}
};
var $mdgriffith$elm_ui$Internal$Model$paddingNameFloat = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(top) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(right) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(bottom) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(left)))))));
	});
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 9:
				return _Utils_update(
					els,
					{
						c: A2($elm$core$List$cons, attr, els.c)
					});
			case 7:
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g),
						o: A2($elm$core$List$cons, attr, els.o),
						c: A2($elm$core$List$cons, attr, els.c)
					}) : (stacked ? _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					}) : _Utils_update(
					els,
					{
						c: A2($elm$core$List$cons, attr, els.c)
					}));
			case 8:
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g),
						c: A2($elm$core$List$cons, attr, els.c)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g),
						c: A2($elm$core$List$cons, attr, els.c)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						c: A2($elm$core$List$cons, attr, els.c)
					}) : _Utils_update(
					els,
					{
						c: A2($elm$core$List$cons, attr, els.c)
					})));
			case 6:
				return _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					});
			case 5:
				return _Utils_update(
					els,
					{
						g: A2($elm$core$List$cons, attr, els.g)
					});
			case 4:
				switch (attr.b.$) {
					case 5:
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								g: A2($elm$core$List$cons, attr, els.g),
								o: A2($elm$core$List$cons, attr, els.o),
								c: A2($elm$core$List$cons, attr, els.c),
								aJ: A2($elm$core$List$cons, attr, els.aJ)
							});
					case 7:
						var cls = attr.a;
						var _v2 = attr.b;
						var pad = _v2.a;
						var t = _v2.b;
						var r = _v2.c;
						var b = _v2.d;
						var l = _v2.e;
						if (isMultiline) {
							return _Utils_update(
								els,
								{
									w: A2($elm$core$List$cons, attr, els.w),
									c: A2($elm$core$List$cons, attr, els.c)
								});
						} else {
							var newTop = t - A2($elm$core$Basics$min, t, b);
							var newLineHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'line-height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newBottom = b - A2($elm$core$Basics$min, t, b);
							var reducedVerticalPadding = A2(
								$mdgriffith$elm_ui$Internal$Model$StyleClass,
								$mdgriffith$elm_ui$Internal$Flag$padding,
								A5(
									$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
									A4($mdgriffith$elm_ui$Internal$Model$paddingNameFloat, newTop, r, newBottom, l),
									newTop,
									r,
									newBottom,
									l));
							return _Utils_update(
								els,
								{
									w: A2($elm$core$List$cons, attr, els.w),
									o: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.o)),
									c: A2($elm$core$List$cons, reducedVerticalPadding, els.c)
								});
						}
					case 6:
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								w: A2($elm$core$List$cons, attr, els.w),
								c: A2($elm$core$List$cons, attr, els.c)
							});
					case 10:
						return _Utils_update(
							els,
							{
								w: A2($elm$core$List$cons, attr, els.w),
								c: A2($elm$core$List$cons, attr, els.c)
							});
					case 2:
						return _Utils_update(
							els,
							{
								g: A2($elm$core$List$cons, attr, els.g)
							});
					case 1:
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								g: A2($elm$core$List$cons, attr, els.g)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								c: A2($elm$core$List$cons, attr, els.c)
							});
				}
			case 0:
				return els;
			case 1:
				var a = attr.a;
				return _Utils_update(
					els,
					{
						o: A2($elm$core$List$cons, attr, els.o)
					});
			case 2:
				return _Utils_update(
					els,
					{
						o: A2($elm$core$List$cons, attr, els.o)
					});
			case 3:
				return _Utils_update(
					els,
					{
						c: A2($elm$core$List$cons, attr, els.c)
					});
			default:
				return _Utils_update(
					els,
					{
						o: A2($elm$core$List$cons, attr, els.o)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				w: $elm$core$List$reverse(redist.w),
				g: $elm$core$List$reverse(redist.g),
				o: $elm$core$List$reverse(redist.o),
				c: $elm$core$List$reverse(redist.c),
				aJ: $elm$core$List$reverse(redist.aJ)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{w: _List_Nil, g: _List_Nil, o: _List_Nil, c: _List_Nil, aJ: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.am;
	var right = _v0.al;
	var bottom = _v0._;
	var left = _v0.af;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Element$Input$charcoal = A3($mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
var $mdgriffith$elm_ui$Element$Input$renderPlaceholder = F3(
	function (_v0, forPlaceholder, on) {
		var placeholderAttrs = _v0.a;
		var placeholderEl = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				forPlaceholder,
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$charcoal),
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.ct + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.eN)),
							$mdgriffith$elm_ui$Element$clip,
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$Background$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$alpha(
							on ? 1 : 0)
						]),
					placeholderAttrs)),
			placeholderEl);
	});
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.eY);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.u, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions.aR),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 5)) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.u;
			if (!_v7.$) {
				var inputType = _v7.a;
				return false;
			} else {
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$mdgriffith$elm_ui$Element$Input$isConstrained,
						$elm$core$List$head(
							$elm$core$List$reverse(
								A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Element$Input$getHeight, withDefaults)))));
			}
		}();
		var getPadding = function (attr) {
			if ((attr.$ === 4) && (attr.b.$ === 7)) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						_: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						af: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						al: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						am: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(t - 3))
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var parentPadding = A2(
			$elm$core$Maybe$withDefault,
			{_: 0, af: 0, al: 0, am: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.u;
				if (!_v3.$) {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.u;
					if (!_v4.$) {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.es)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.eo),
								$mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding(withDefaults),
								$mdgriffith$elm_ui$Element$paddingEach(parentPadding),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2(
									$elm$html$Html$Attributes$style,
									'margin',
									$mdgriffith$elm_ui$Element$Input$renderBox(
										$mdgriffith$elm_ui$Element$Input$negateBox(parentPadding)))),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2($elm$html$Html$Attributes$style, 'box-sizing', 'content-box'))
							]);
					}
				}(),
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Input$value(textOptions.c1),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.cw)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.aR),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.J),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.E))
						]),
					redistributed.o)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.u;
			if (_v0.$ === 1) {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.b9),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.er)
								])),
						redistributed.c),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asParagraph,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$inFront(inputElement),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.eq),
												redistributed.aJ)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.c1 === '') {
											var _v1 = textOptions.cD;
											if (_v1.$ === 1) {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.c1 === '')
													]);
											}
										} else {
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Internal$Model$unstyled(
													A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.ep)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.c1 + '\u00A0')
															])))
												]);
										}
									}()))
							])));
			} else {
				var inputType = _v0.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						A2(
							$elm$core$List$cons,
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.b9),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.c,
										function () {
										var _v2 = textOptions.cD;
										if (_v2.$ === 1) {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.w, textOptions.c1 === ''))
												]);
										}
									}()
									])))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[inputElement])));
			}
		}();
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.d_),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.aR) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.g))),
			textOptions.aR,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		E: $elm$core$Maybe$Nothing,
		J: false,
		u: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $mdgriffith$elm_ui$Element$Font$underline = $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.fz);
var $author$project$Screens$Welcome$welcomeScreenView = function (_v0) {
	var manualLocationErr = _v0.ag;
	var manualLocation = _v0.aC;
	var errorMessage = _v0.d7;
	var errStr = A2($elm$core$Maybe$withDefault, '', errorMessage);
	var _v1 = manualLocation;
	var lat = _v1.a;
	var lon = _v1.b;
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
			]),
		A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
					$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary)
				]),
			A2(
				$mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						$mdgriffith$elm_ui$Element$centerX,
						$mdgriffith$elm_ui$Element$centerY
					]),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Element$paragraph,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$Font$center,
								$mdgriffith$elm_ui$Element$Font$size(52),
								$mdgriffith$elm_ui$Element$Font$semiBold
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$Font$center]),
								$mdgriffith$elm_ui$Element$text('Welcome to')),
								$author$project$Utils$br,
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$Font$heavy]),
								$mdgriffith$elm_ui$Element$text('WeatherMate'))
							])),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$author$project$Utils$paddingTop(18),
								$mdgriffith$elm_ui$Element$centerX
							]),
						A2(
							$mdgriffith$elm_ui$Element$Input$button,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$centerX,
									$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black),
									$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary),
									$mdgriffith$elm_ui$Element$Font$bold,
									A2($mdgriffith$elm_ui$Element$paddingXY, 24, 12),
									$mdgriffith$elm_ui$Element$Font$size(22),
									_Utils_eq(errStr, $author$project$Utils$noGeoApiAvailableErrStr) ? $mdgriffith$elm_ui$Element$Font$strike : $mdgriffith$elm_ui$Element$Font$bold
								]),
							{
								aR: $mdgriffith$elm_ui$Element$text('Enable Location Permission'),
								bd: $elm$core$Maybe$Just($author$project$Screens$Welcome$RequestLocationPerms)
							})),
						A2(
						$mdgriffith$elm_ui$Element$el,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$Font$bold,
								$mdgriffith$elm_ui$Element$centerX,
								A2($mdgriffith$elm_ui$Element$paddingXY, 0, 15)
							]),
						$mdgriffith$elm_ui$Element$text('or')),
						A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$centerX,
								$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black)
							]),
						_List_fromArray(
							[
								(manualLocationErr !== '') ? A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$paragraph,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Element$paddingXY, 24, 12),
												$mdgriffith$elm_ui$Element$Font$center,
												$mdgriffith$elm_ui$Element$spacing(8)
											]),
										_List_fromArray(
											[
												A2(
												$mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$centerX,
														$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary),
														$mdgriffith$elm_ui$Element$Font$heavy,
														$mdgriffith$elm_ui$Element$Font$underline,
														$mdgriffith$elm_ui$Element$Font$size(22)
													]),
												$mdgriffith$elm_ui$Element$text('Error')),
												$author$project$Utils$br,
												A2(
												$mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$centerX,
														$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary),
														$mdgriffith$elm_ui$Element$Font$light,
														$mdgriffith$elm_ui$Element$Font$size(18)
													]),
												$mdgriffith$elm_ui$Element$text(manualLocationErr))
											])),
										A2(
										$mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$height(
												$mdgriffith$elm_ui$Element$px(1)),
												$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary)
											]),
										$mdgriffith$elm_ui$Element$none)
									])) : $mdgriffith$elm_ui$Element$none,
								A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Element$paddingXY, 24, 24),
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
										$mdgriffith$elm_ui$Element$spacing(24)
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$Input$text,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary)
											]),
										{
											aR: A2(
												$mdgriffith$elm_ui$Element$Input$labelAbove,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary)
													]),
												$mdgriffith$elm_ui$Element$text('Latitude:')),
											cw: function (l) {
												return $author$project$Screens$Welcome$OnChangeLatitude(l);
											},
											cD: $elm$core$Maybe$Just(
												A2(
													$mdgriffith$elm_ui$Element$Input$placeholder,
													_List_Nil,
													A2(
														$mdgriffith$elm_ui$Element$el,
														_List_fromArray(
															[
																$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$black)
															]),
														$mdgriffith$elm_ui$Element$text('-150.58')))),
											c1: lat
										}),
										A2(
										$mdgriffith$elm_ui$Element$Input$text,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary)
											]),
										{
											aR: A2(
												$mdgriffith$elm_ui$Element$Input$labelAbove,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary)
													]),
												$mdgriffith$elm_ui$Element$text('Longitude:')),
											cw: function (l) {
												return $author$project$Screens$Welcome$OnChangeLongitude(l);
											},
											cD: $elm$core$Maybe$Just(
												A2(
													$mdgriffith$elm_ui$Element$Input$placeholder,
													_List_Nil,
													A2(
														$mdgriffith$elm_ui$Element$el,
														_List_fromArray(
															[
																$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$black)
															]),
														$mdgriffith$elm_ui$Element$text('75.88')))),
											c1: lon
										})
									])),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
										$mdgriffith$elm_ui$Element$height(
										$mdgriffith$elm_ui$Element$px(1)),
										$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$primary)
									]),
								$mdgriffith$elm_ui$Element$none),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$centerX]),
								A2(
									$mdgriffith$elm_ui$Element$Input$button,
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Element$paddingXY, 24, 12),
											$mdgriffith$elm_ui$Element$centerX,
											$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary),
											$mdgriffith$elm_ui$Element$Font$bold,
											$mdgriffith$elm_ui$Element$Font$size(22)
										]),
									{
										aR: $mdgriffith$elm_ui$Element$text('Enter coordinates manually'),
										bd: $elm$core$Maybe$Just($author$project$Screens$Welcome$SubmitManualLocationForm)
									}))
							]))
					]))));
};
var $author$project$Main$view = function (model) {
	return A2(
		$mdgriffith$elm_ui$Element$layout,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$typeface('Inter'),
						$mdgriffith$elm_ui$Element$Font$sansSerif
					])),
				$mdgriffith$elm_ui$Element$inFront(
				function () {
					switch (model.$) {
						case 2:
							var modelData = model.a;
							var divider = A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
										$mdgriffith$elm_ui$Element$height(
										$mdgriffith$elm_ui$Element$px(1)),
										$mdgriffith$elm_ui$Element$Background$color(modelData.t)
									]),
								$mdgriffith$elm_ui$Element$none);
							return modelData.aB ? A2(
								$mdgriffith$elm_ui$Element$column,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
										$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black)
									]),
								_List_fromArray(
									[
										A2(
										$mdgriffith$elm_ui$Element$row,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
												A2($mdgriffith$elm_ui$Element$paddingXY, 15, 8)
											]),
										_List_fromArray(
											[
												A2(
												$mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
														$mdgriffith$elm_ui$Element$Font$color(modelData.t),
														$mdgriffith$elm_ui$Element$Font$heavy
													]),
												$mdgriffith$elm_ui$Element$text('Primary Color')),
												$mdgriffith$elm_ui$Element$html(
												A2(
													$elm$html$Html$input,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$type_('color'),
															A2($elm$html$Html$Attributes$style, 'border', 'transparent'),
															A2($elm$html$Html$Attributes$style, 'background', 'transparent'),
															A2($elm$html$Html$Attributes$style, 'height', '35px'),
															$elm$html$Html$Attributes$value(
															function (_v1) {
																var blue = _v1.dH;
																var green = _v1.ec;
																var red = _v1.eQ;
																return A2(
																	$elm$core$String$join,
																	'',
																	A2(
																		$elm$core$List$cons,
																		'#',
																		A2(
																			$elm$core$List$map,
																			$author$project$Main$toHex,
																			_List_fromArray(
																				[
																					$elm$core$Basics$round(red * 255),
																					$elm$core$Basics$round(green * 255),
																					$elm$core$Basics$round(blue * 255)
																				]))));
															}(
																$mdgriffith$elm_ui$Element$toRgb(modelData.t))),
															A2($elm$html$Html$Attributes$style, 'width', '35px'),
															$elm$html$Html$Events$onInput(
															function (l) {
																return $author$project$Main$OnMainScreenMsg(
																	$author$project$Main$ChangedPrimaryColor(l));
															})
														]),
													_List_Nil))
											])),
										divider,
										A2(
										$mdgriffith$elm_ui$Element$row,
										_List_fromArray(
											[
												$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
											]),
										_List_fromArray(
											[
												A2(
												$mdgriffith$elm_ui$Element$Input$button,
												_List_fromArray(
													[
														$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
														$mdgriffith$elm_ui$Element$Font$color(modelData.t),
														$mdgriffith$elm_ui$Element$padding(15),
														$mdgriffith$elm_ui$Element$Font$heavy,
														$mdgriffith$elm_ui$Element$Font$center
													]),
												{
													aR: $mdgriffith$elm_ui$Element$text('X'),
													bd: $elm$core$Maybe$Just(
														$author$project$Main$OnMainScreenMsg($author$project$Main$CloseOptionsMenu))
												})
											])),
										divider
									])) : $mdgriffith$elm_ui$Element$none;
						case 1:
							return $mdgriffith$elm_ui$Element$none;
						default:
							var noLocationDataModel = model.a;
							var _v2 = noLocationDataModel.d7;
							if (!_v2.$) {
								var errStr = _v2.a;
								return A2(
									$mdgriffith$elm_ui$Element$paragraph,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$Background$color($author$project$Utils$black),
											$mdgriffith$elm_ui$Element$Font$color($author$project$Utils$primary),
											$mdgriffith$elm_ui$Element$Font$bold,
											A2($mdgriffith$elm_ui$Element$paddingXY, 24, 12),
											$mdgriffith$elm_ui$Element$Font$size(22)
										]),
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$text('Error: '),
											A2(
											$mdgriffith$elm_ui$Element$el,
											_List_fromArray(
												[$mdgriffith$elm_ui$Element$Font$light]),
											$mdgriffith$elm_ui$Element$text(errStr))
										]));
							} else {
								return $mdgriffith$elm_ui$Element$none;
							}
					}
				}())
			]),
		function () {
			switch (model.$) {
				case 0:
					var m = model.a;
					return A2(
						$mdgriffith$elm_ui$Element$map,
						$author$project$Main$OnWelcomeScreenMsg,
						$author$project$Screens$Welcome$welcomeScreenView(m));
				case 1:
					var m = model.a;
					return $author$project$Main$loadingScreenView(m);
				default:
					var m = model.a;
					return A2(
						$mdgriffith$elm_ui$Element$map,
						$author$project$Main$OnMainScreenMsg,
						$author$project$Main$mainScreen(m));
			}
		}());
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{en: $author$project$Main$init, fd: $author$project$Main$subscriptions, fA: $author$project$Main$update, fB: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));