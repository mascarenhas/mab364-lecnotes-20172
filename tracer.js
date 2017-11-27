const Fat = {
    apply(n) {
        if(n < 2)
            return 1
        else
            return n * this.apply(n-1)
    }
}
function tracer(f) {
    const o = {
        apply(x) {
            console.log(x);
            return super.apply(x)
        }
    }
    Object.setPrototypeOf(o, f)
    return o
}
console.log(Fat.apply(5))
console.log(tracer(Fat).apply(5))
