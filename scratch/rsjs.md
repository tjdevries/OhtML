
```html
<section class="counter" data-counter> <!-- (1) -->
  <output id="my-output" data-counter-output>0</output> <!-- (2) -->
  <button class="increment-btn" data-counter-increment>Increment</button>
</section>
```

1. Invoke a JavaScript behavior with a data attribute.
2. Mark relevant descendant elements.

```javascript
// counter.js (1)
document.querySelectorAll("[data-counter]")  // (2)
  .forEach(el => {
    const
    output = el.querySelector("[data-counter-output]"),
    increment = el.querySelector("[data-counter-increment]"); // (3)

    increment.addEventListener("click", e => output.textContent++); // (4)
  });
```
1. File should have the same name as the data attribute, so that we can locate it easily.
2. Get all elements that invoke this behavior.
3. Get any child elements we need.
4. Register event handlers.
