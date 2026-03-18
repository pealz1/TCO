import { describe, it, expect, vi, beforeEach } from 'vitest';
import { Store, store } from '../src/renderer/store.js';

describe('Store', () => {
  let s;

  beforeEach(() => {
    s = new Store({ count: 0, name: 'test', items: [] });
  });

  describe('initial state', () => {
    it('has correct default keys on singleton', () => {
      const state = store.getState();
      expect(state).toHaveProperty('folders');
      expect(state).toHaveProperty('notes');
      expect(state).toHaveProperty('activeFolder', 'all');
      expect(state).toHaveProperty('activeNote', null);
      expect(state).toHaveProperty('config');
      expect(state).toHaveProperty('view', 'main');
      expect(state).toHaveProperty('searchQuery', '');
      expect(state).toHaveProperty('sidebarCollapsed', false);
      expect(state).toHaveProperty('theme', 'light');
      expect(state).toHaveProperty('tags');
    });

    it('folders, notes, tags default to empty arrays', () => {
      const state = store.getState();
      expect(Array.isArray(state.folders)).toBe(true);
      expect(Array.isArray(state.notes)).toBe(true);
      expect(Array.isArray(state.tags)).toBe(true);
    });

    it('config defaults to empty object', () => {
      const state = store.getState();
      expect(state.config).toEqual({});
    });
  });

  describe('get()', () => {
    it('retrieves a value by key', () => {
      expect(s.get('count')).toBe(0);
      expect(s.get('name')).toBe('test');
    });

    it('returns undefined for unknown key', () => {
      expect(s.get('nonexistent')).toBeUndefined();
    });
  });

  describe('set()', () => {
    it('updates a value by key', () => {
      s.set('count', 42);
      expect(s.get('count')).toBe(42);
    });

    it('fires registered listeners when value changes', () => {
      const cb = vi.fn();
      s.on('count', cb);
      s.set('count', 5);
      expect(cb).toHaveBeenCalledOnce();
      expect(cb).toHaveBeenCalledWith(5, 0);
    });

    it('does not fire listeners when value is the same (shallow equality)', () => {
      const cb = vi.fn();
      s.on('count', cb);
      s.set('count', 0);
      expect(cb).not.toHaveBeenCalled();
    });

    it('does not fire listeners for a different key', () => {
      const cb = vi.fn();
      s.on('name', cb);
      s.set('count', 99);
      expect(cb).not.toHaveBeenCalled();
    });
  });

  describe('on()', () => {
    it('subscribes to key changes and fires on update', () => {
      const cb = vi.fn();
      s.on('name', cb);
      s.set('name', 'hello');
      expect(cb).toHaveBeenCalledWith('hello', 'test');
    });

    it('fires multiple listeners on the same key', () => {
      const cb1 = vi.fn();
      const cb2 = vi.fn();
      s.on('count', cb1);
      s.on('count', cb2);
      s.set('count', 7);
      expect(cb1).toHaveBeenCalledOnce();
      expect(cb2).toHaveBeenCalledOnce();
    });

    it('returns an unsubscribe function', () => {
      const cb = vi.fn();
      const unsub = s.on('count', cb);
      expect(typeof unsub).toBe('function');
      unsub();
      s.set('count', 10);
      expect(cb).not.toHaveBeenCalled();
    });
  });

  describe('off()', () => {
    it('unsubscribes a callback from a key', () => {
      const cb = vi.fn();
      s.on('count', cb);
      s.off('count', cb);
      s.set('count', 3);
      expect(cb).not.toHaveBeenCalled();
    });

    it('does not affect other listeners on the same key', () => {
      const cb1 = vi.fn();
      const cb2 = vi.fn();
      s.on('count', cb1);
      s.on('count', cb2);
      s.off('count', cb1);
      s.set('count', 4);
      expect(cb1).not.toHaveBeenCalled();
      expect(cb2).toHaveBeenCalledOnce();
    });

    it('is a no-op for an unregistered callback', () => {
      expect(() => s.off('count', vi.fn())).not.toThrow();
    });
  });

  describe('getState()', () => {
    it('returns an object matching current state', () => {
      const state = s.getState();
      expect(state.count).toBe(0);
      expect(state.name).toBe('test');
    });

    it('returns a copy, not the internal reference', () => {
      const state = s.getState();
      state.count = 999;
      expect(s.get('count')).toBe(0);
    });

    it('returns a new object on each call', () => {
      const a = s.getState();
      const b = s.getState();
      expect(a).not.toBe(b);
    });
  });
});
