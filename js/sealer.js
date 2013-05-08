var sealer = function () {
    "use strict";
    var boxes = [], values = [];
    return {
        seal: function (value) {
            var box = {}, i = boxes.length;

            boxes[i] = box;
            values[i] = value;

            return box;
        },
        unseal: function (box) {
            return values[boxes.indexOf(box)];
        }
    };
};

var inspect = function (obj) {
    "use strict";
    console.log(obj);
};

var aseal = sealer();

var box = aseal.seal('hello');
inspect(box);

var value = aseal.unseal(box);
console.log(value);