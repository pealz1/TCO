export function debounce(fn, ms) {
  let timer = null;

  function wrapper(...args) {
    clearTimeout(timer);
    timer = setTimeout(() => {
      timer = null;
      fn.apply(this, args);
    }, ms);
  }

  wrapper.cancel = function () {
    clearTimeout(timer);
    timer = null;
  };

  return wrapper;
}

export function throttle(fn, ms) {
  let lastCall = 0;
  let timer = null;

  function wrapper(...args) {
    const now = Date.now();
    const remaining = ms - (now - lastCall);

    if (remaining <= 0) {
      if (timer) {
        clearTimeout(timer);
        timer = null;
      }
      lastCall = now;
      fn.apply(this, args);
    } else {
      clearTimeout(timer);
      timer = setTimeout(() => {
        lastCall = Date.now();
        timer = null;
        fn.apply(this, args);
      }, remaining);
    }
  }

  wrapper.cancel = function () {
    clearTimeout(timer);
    timer = null;
    lastCall = 0;
  };

  return wrapper;
}
