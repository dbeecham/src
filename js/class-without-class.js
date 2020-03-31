const myclass = (args) => {

    const priv = 3;
    const priv_func = () => {
        console.log('called priv_func');
    };

    return {
        func: (arg) => {
            console.log('called func');
            priv_func();
        }
    };
};

const main = () => {
    c = myclass(3);
    c.func();
}

main();
