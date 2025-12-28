// SPL Pythagorean Triples Example
// Demonstrates nested generators with ZipResult and Flatten

#include <spl/spl.h>
#include <iostream>

constexpr auto pythagorean_triples(){
 return spl::compose(spl::iota(1),  // c: 1, 2, 3, ...
        spl::zip_result([](size_t c) { return spl::iota(1, c + 1); }),  // a: 1..c
        spl::flatten(),
        spl::zip_result([](size_t c, size_t a) { return spl::iota(a, c + 1); }),  // b: a..c
        spl::flatten(),
        spl::filter([](size_t c, size_t a, size_t b) {
            return a * a + b * b == c * c;
        }),
        spl::swizzle<1, 2, 0>()  // Reorder from (c, a, b) to (a, b, c)
    );
    


}

int main() {
    std::cout << "=== Pythagorean Triples ===\n";
    std::cout << "First 10 Pythagorean triples (a, b, c) where a^2 + b^2 = c^2:\n\n";

   spl::apply(
        pythagorean_triples(),
        spl::take(10),
        spl::for_each([](size_t a, size_t b, size_t c) {
            std::cout << a << "^2 + " << b << "^2 = " << c << "^2"
                      << "  (" << a << ", " << b << ", " << c << ")\n";
        })
    );

    // Expected output:
    // 3^2 + 4^2 = 5^2    (3, 4, 5)
    // 6^2 + 8^2 = 10^2   (6, 8, 10)
    // 5^2 + 12^2 = 13^2  (5, 12, 13)
    // 9^2 + 12^2 = 15^2  (9, 12, 15)
    // 8^2 + 15^2 = 17^2  (8, 15, 17)
    // 12^2 + 16^2 = 20^2 (12, 16, 20)
    // 7^2 + 24^2 = 25^2  (7, 24, 25)
    // 15^2 + 20^2 = 25^2 (15, 20, 25)
    // 10^2 + 24^2 = 26^2 (10, 24, 26)
    // 20^2 + 21^2 = 29^2 (20, 21, 29)

    return 0;
}
