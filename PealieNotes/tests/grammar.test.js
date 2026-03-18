import { describe, it, expect } from 'vitest';
import { checkGrammar } from '../src/renderer/grammar.js';

describe('checkGrammar', () => {
  describe('double words', () => {
    it('should detect repeated words', () => {
      const issues = checkGrammar('the the cat sat');
      const dw = issues.find(i => i.message.includes('Repeated word'));
      expect(dw).toBeTruthy();
      expect(dw.suggestions[0]).toBe('the');
      expect(dw.offset).toBe(0);
      expect(dw.length).toBe(7);
    });

    it('should detect case-insensitive double words', () => {
      const issues = checkGrammar('The the cat');
      const dw = issues.find(i => i.message.includes('Repeated word'));
      expect(dw).toBeTruthy();
    });

    it('should not flag different words', () => {
      const issues = checkGrammar('the cat sat');
      const dw = issues.find(i => i.message.includes('Repeated word'));
      expect(dw).toBeUndefined();
    });
  });

  describe('capitalization', () => {
    it('should detect lowercase after period', () => {
      const issues = checkGrammar('Hello world. this is a test.');
      const cap = issues.find(i => i.message.includes('capital letter') && i.offset > 0);
      expect(cap).toBeTruthy();
      expect(cap.suggestions[0]).toBe('T');
    });

    it('should detect lowercase at start of text', () => {
      const issues = checkGrammar('hello world.');
      const cap = issues.find(i => i.message.includes('capital letter') && i.offset === 0);
      expect(cap).toBeTruthy();
      expect(cap.suggestions[0]).toBe('H');
    });

    it('should not flag properly capitalized text', () => {
      const issues = checkGrammar('Hello world. This is good.');
      const cap = issues.find(i => i.message.includes('capital letter'));
      expect(cap).toBeUndefined();
    });
  });

  describe('double spaces', () => {
    it('should detect double spaces', () => {
      const issues = checkGrammar('hello  world');
      const ds = issues.find(i => i.message.includes('Multiple spaces'));
      expect(ds).toBeTruthy();
      expect(ds.suggestions[0]).toBe(' ');
    });

    it('should detect triple spaces', () => {
      const issues = checkGrammar('hello   world');
      const ds = issues.find(i => i.message.includes('Multiple spaces'));
      expect(ds).toBeTruthy();
      expect(ds.length).toBe(3);
    });

    it('should not flag single spaces', () => {
      const issues = checkGrammar('hello world');
      const ds = issues.find(i => i.message.includes('Multiple spaces'));
      expect(ds).toBeUndefined();
    });
  });

  describe('common typos', () => {
    it('should detect "recieve"', () => {
      const issues = checkGrammar('I recieve the package');
      const typo = issues.find(i => i.type === 'typo' && i.message.includes('recieve'));
      expect(typo).toBeTruthy();
      expect(typo.suggestions[0]).toBe('receive');
    });

    it('should detect multiple typos', () => {
      const issues = checkGrammar('teh cat is seperate');
      const typos = issues.filter(i => i.type === 'typo');
      expect(typos.length).toBeGreaterThanOrEqual(2);
      expect(typos.find(i => i.message.includes('teh')).suggestions[0]).toBe('the');
      expect(typos.find(i => i.message.includes('seperate')).suggestions[0]).toBe('separate');
    });
  });

  describe('edge cases', () => {
    it('should return empty array for empty string', () => {
      expect(checkGrammar('')).toEqual([]);
    });

    it('should return empty array for correct text', () => {
      expect(checkGrammar('This is correct. Everything looks good.')).toEqual([]);
    });

    it('should detect multiple issue types', () => {
      const issues = checkGrammar('teh  cat. the the dog. hello');
      expect(issues.length).toBeGreaterThanOrEqual(3);
      expect(issues.some(i => i.type === 'typo')).toBe(true);
      expect(issues.some(i => i.message.includes('Multiple spaces'))).toBe(true);
      expect(issues.some(i => i.message.includes('Repeated word'))).toBe(true);
    });

    it('should sort issues by offset', () => {
      const issues = checkGrammar('teh  cat the the');
      for (let i = 1; i < issues.length; i++) {
        expect(issues[i].offset).toBeGreaterThanOrEqual(issues[i - 1].offset);
      }
    });
  });
});
