import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { generateId } from '../src/renderer/utils/ids.js';
import { debounce, throttle } from '../src/renderer/utils/debounce.js';
import { formatDate, formatFileSize, truncate } from '../src/renderer/utils/format.js';

// ─── ids ─────────────────────────────────────────────────────────────────────

describe('generateId', () => {
  it('returns a string', () => {
    expect(typeof generateId()).toBe('string');
  });

  it('returns a valid UUID v4 format', () => {
    const id = generateId();
    expect(id).toMatch(
      /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i
    );
  });

  it('returns unique values on each call', () => {
    const ids = new Set(Array.from({ length: 100 }, () => generateId()));
    expect(ids.size).toBe(100);
  });
});

// ─── debounce ────────────────────────────────────────────────────────────────

describe('debounce', () => {
  beforeEach(() => vi.useFakeTimers());
  afterEach(() => vi.useRealTimers());

  it('delays execution until after the wait period', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 200);

    debouncedFn();
    expect(fn).not.toHaveBeenCalled();

    vi.advanceTimersByTime(200);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('only fires once when called rapidly', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    for (let i = 0; i < 10; i++) {
      debouncedFn();
    }
    vi.advanceTimersByTime(100);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('uses the last set of arguments', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    debouncedFn('a');
    debouncedFn('b');
    debouncedFn('c');
    vi.advanceTimersByTime(100);
    expect(fn).toHaveBeenCalledWith('c');
  });

  it('resets the timer on each call', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    debouncedFn();
    vi.advanceTimersByTime(50);
    debouncedFn();
    vi.advanceTimersByTime(50);
    expect(fn).not.toHaveBeenCalled();

    vi.advanceTimersByTime(50);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('cancel() prevents pending execution', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    debouncedFn();
    debouncedFn.cancel();
    vi.advanceTimersByTime(200);
    expect(fn).not.toHaveBeenCalled();
  });

  it('can fire again after cancel()', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    debouncedFn();
    debouncedFn.cancel();
    debouncedFn();
    vi.advanceTimersByTime(100);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('fires multiple times with sufficient gaps', () => {
    const fn = vi.fn();
    const debouncedFn = debounce(fn, 100);

    debouncedFn();
    vi.advanceTimersByTime(100);
    debouncedFn();
    vi.advanceTimersByTime(100);
    expect(fn).toHaveBeenCalledTimes(2);
  });
});

// ─── throttle ────────────────────────────────────────────────────────────────

describe('throttle', () => {
  beforeEach(() => vi.useFakeTimers());
  afterEach(() => vi.useRealTimers());

  it('fires immediately on first call', () => {
    const fn = vi.fn();
    const throttledFn = throttle(fn, 100);

    throttledFn();
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('ignores calls within the interval', () => {
    const fn = vi.fn();
    const throttledFn = throttle(fn, 100);

    throttledFn();
    throttledFn();
    throttledFn();
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it('fires again after the interval elapses', () => {
    const fn = vi.fn();
    const throttledFn = throttle(fn, 100);

    throttledFn();
    vi.advanceTimersByTime(100);
    throttledFn();
    expect(fn).toHaveBeenCalledTimes(2);
  });

  it('cancel() prevents any trailing call', () => {
    const fn = vi.fn();
    const throttledFn = throttle(fn, 100);

    throttledFn();
    throttledFn('trailing');
    throttledFn.cancel();
    vi.advanceTimersByTime(200);
    expect(fn).toHaveBeenCalledTimes(1);
  });
});

// ─── formatDate ──────────────────────────────────────────────────────────────

describe('formatDate', () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date('2026-03-17T12:00:00.000Z'));
  });
  afterEach(() => vi.useRealTimers());

  it('returns "Just now" for dates less than 60 seconds ago', () => {
    const d = new Date(Date.now() - 30 * 1000);
    expect(formatDate(d)).toBe('Just now');
  });

  it('returns "Just now" for the exact current time', () => {
    expect(formatDate(new Date())).toBe('Just now');
  });

  it('returns "X min ago" for dates within the last hour', () => {
    const d = new Date(Date.now() - 15 * 60 * 1000);
    expect(formatDate(d)).toBe('15 min ago');
  });

  it('returns "1 min ago" for 1 minute past', () => {
    const d = new Date(Date.now() - 60 * 1000);
    expect(formatDate(d)).toBe('1 min ago');
  });

  it('returns "X hr ago" for dates within the last 24 hours', () => {
    const d = new Date(Date.now() - 3 * 60 * 60 * 1000);
    expect(formatDate(d)).toBe('3 hr ago');
  });

  it('returns "1 hr ago" for exactly 1 hour past', () => {
    const d = new Date(Date.now() - 60 * 60 * 1000);
    expect(formatDate(d)).toBe('1 hr ago');
  });

  it('returns "Yesterday" for a date on the previous calendar day', () => {
    const d = new Date('2026-03-16T10:00:00.000Z');
    expect(formatDate(d)).toBe('Yesterday');
  });

  it('returns formatted date string for older dates', () => {
    const d = new Date('2026-01-05T08:00:00.000Z');
    const result = formatDate(d);
    expect(result).toMatch(/Jan\s+5,\s+2026/);
  });

  it('accepts a timestamp number', () => {
    const ts = Date.now() - 5000;
    expect(formatDate(ts)).toBe('Just now');
  });

  it('returns empty string for null', () => {
    expect(formatDate(null)).toBe('');
  });

  it('returns empty string for undefined', () => {
    expect(formatDate(undefined)).toBe('');
  });

  it('returns empty string for an invalid date string', () => {
    expect(formatDate('not-a-date')).toBe('');
  });
});

// ─── formatFileSize ───────────────────────────────────────────────────────────

describe('formatFileSize', () => {
  it('returns "0 B" for 0 bytes', () => {
    expect(formatFileSize(0)).toBe('0 B');
  });

  it('formats bytes', () => {
    expect(formatFileSize(500)).toBe('500 B');
  });

  it('formats kilobytes', () => {
    expect(formatFileSize(1200)).toBe('1.2 KB');
  });

  it('formats megabytes', () => {
    expect(formatFileSize(3_400_000)).toBe('3.4 MB');
  });

  it('formats gigabytes', () => {
    expect(formatFileSize(2_500_000_000)).toBe('2.5 GB');
  });

  it('formats terabytes', () => {
    expect(formatFileSize(1_100_000_000_000)).toBe('1.1 TB');
  });

  it('rounds to one decimal place', () => {
    expect(formatFileSize(1500)).toBe('1.5 KB');
  });

  it('returns empty string for null', () => {
    expect(formatFileSize(null)).toBe('');
  });

  it('returns empty string for negative bytes', () => {
    expect(formatFileSize(-1)).toBe('');
  });
});

// ─── truncate ────────────────────────────────────────────────────────────────

describe('truncate', () => {
  it('returns the string unchanged when shorter than len', () => {
    expect(truncate('hello', 10)).toBe('hello');
  });

  it('returns the string unchanged when equal to len', () => {
    expect(truncate('hello', 5)).toBe('hello');
  });

  it('truncates and appends "..." when longer than len', () => {
    expect(truncate('hello world', 5)).toBe('hello...');
  });

  it('handles an empty string', () => {
    expect(truncate('', 5)).toBe('');
  });

  it('returns empty string for null', () => {
    expect(truncate(null, 5)).toBe('');
  });

  it('returns empty string for undefined', () => {
    expect(truncate(undefined, 5)).toBe('');
  });

  it('returns empty string when len is 0', () => {
    expect(truncate('hello', 0)).toBe('');
  });

  it('converts non-string values to string before truncating', () => {
    expect(truncate(12345678, 5)).toBe('12345...');
  });

  it('handles a very long string', () => {
    const long = 'a'.repeat(1000);
    const result = truncate(long, 10);
    expect(result).toBe('aaaaaaaaaa...');
    expect(result.length).toBe(13);
  });
});
