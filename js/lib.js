/*jslint browser:true*/
// Thanks to
// Douglas Crockford (crockford.com)
// Dustin Diaz (github.com/ded)
// Diego Perini ()

"use strict";

if (typeof Function.prototype.method !== "function") {
    Function.prototype.method = function (name, func) {
        if (!this.prototype[name]) {
            this.prototype[name] = func;
            return this;
        }
    };
}

// IE8 does not have Object.create
Object.method("create", function (o) {
    var F = function () {};
    F.prototype = o;
    return new F();
});

var typeOf = function (value) {
    var s = typeof value;

    if (s === "object") {
        if (value) {
            if (Object.prototype.toString.call(value) === "[object Array]") {
                s = "array";
            }
        } else {
            s = "null";
        }
    }

    return s;
};

Number.method("integer", function () {
    return Math[this < 0 ? "ceil" : "floor"](this);
});

String.method("trim", function () {
    return this.replace(/^\s+|\s$/g, '');
});

String.method("supplant", function (o) {
    return this.replace(
        /\{([^{}]*)\}/g,
        function (a, b) {
            var r = o[b];
            return typeof r === 'string' || typeof r === 'number' ? r : a;
        }
    );
});

console = console || {};
console.log = console.log || function (text) {
    document.documentElement.innerHTML += text;
};

var domReady = function domReady(func) {
    console.log('running...');
    // Check if already loaded
    if (/^loade|^c/.test(document.readyState)) {
        func();
        console.log('already loaded, executing!');
        return;
    }

    if (document.addEventListener) {
        console.log('addEventListener support...');
        document.addEventListener("DOMContentLoaded", function a() {
            console.log('eventListener executed!');
            document.removeEventListener("DOMContentLoaded", a, false);
            func();
        }, false);
    } else if (document.attachEvent) {
        console.log('attachEvent support...');
        document.attachEvent("onreadystatechange", function a() {
            console.log('statechange...');
            if (/^c/.test(document.readyState)) {
                console.log('attachEvent executed!');
                document.detachEvent("onreadystatechange", a);
                func();
            }
        });
    } else if (document.documentElement.doScroll && window === window.top) {
        console.log('doScroll trick support...')
        (function a() {
            if (/^loade|^c/.test(document.readyState)) {
                console.log('doScroll already loaded executed!');
                func();
                return;
            }

            try {
                document.documentElement.doScroll("left");
            } catch (e) {
                setTimeout(a, 50);
                console.log('doScroll sleeping...');
                return;
            }
            console.log('doScroll executed!');
            func();
        }());
    } else {
        console.log('Backing up to window.onload...');
        window.onload = function () {
            func();
        };
    }
};