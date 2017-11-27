const Vazia = {
    imprime() {},
    map(f) { return this; },
    match(cv, cc) {
        return cv.apply();
    }
};
const Quadrado = {
    apply(x) { return x * x }
}
function Cons(h, t) {
    const obj = {
        imprime() {
            console.log(this.h); this.t.imprime();
        },
        map(f) {
            return new Cons(f.apply(this.h), this.t.map(f))
        },
        match(cv, cc) {
            return cc.apply(this.h, this.t);
        }
    };
    obj.h = h;
    obj.t = t;
    return obj;
}
function imprime(l) {
    l.match({
        apply() { }
    }, {
        apply(h, t) {
            console.log(h);
            imprime(t);
        }
    })
}
const l1 = Cons(2, Cons(3, Vazia))
const l2 = l1.map(Quadrado)
l1.imprime()
l2.imprime()
imprime(l1)
imprime(l2)