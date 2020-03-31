const lmysql = (args) => {

    return new Promise(
        (resolve, reject) => {

            if (args) {
                resolve('ok');
            } else {
                reject('nope');
            }

        }
    );

};

const main = () => {
    lmysql(true).then( result => console.log(result) );
    lmysql(false).then( result => console.log(result) );
};

main();
