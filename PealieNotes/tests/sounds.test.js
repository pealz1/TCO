import { describe, it, expect, vi, beforeEach } from 'vitest';

// Mock AudioContext before importing the module
const mockOscillatorStop = vi.fn();
const mockOscillatorStart = vi.fn();
const mockOscillatorConnect = vi.fn();
const mockGainConnect = vi.fn();
const mockSourceConnect = vi.fn();
const mockSourceStart = vi.fn();
const mockFilterConnect = vi.fn();
const mockMasterGainConnect = vi.fn();

const mockSetValueAtTime = vi.fn();
const mockExpRamp = vi.fn();

function makeMockGain() {
  return {
    gain: {
      value: 0,
      setValueAtTime: mockSetValueAtTime,
      exponentialRampToValueAtTime: mockExpRamp,
    },
    connect: mockGainConnect,
  };
}

function makeMockOscillator() {
  return {
    type: '',
    frequency: {
      value: 0,
      setValueAtTime: mockSetValueAtTime,
      exponentialRampToValueAtTime: mockExpRamp,
    },
    connect: mockOscillatorConnect,
    start: mockOscillatorStart,
    stop: mockOscillatorStop,
  };
}

function makeMockFilter() {
  return {
    type: '',
    frequency: {
      value: 0,
      setValueAtTime: mockSetValueAtTime,
      exponentialRampToValueAtTime: mockExpRamp,
    },
    Q: { value: 0 },
    connect: mockFilterConnect,
  };
}

function makeMockBufferSource(buffer) {
  return {
    buffer: null,
    connect: mockSourceConnect,
    start: mockSourceStart,
  };
}

const mockDestination = {};
let mockCurrentTime = 0;

const mockAudioContext = vi.fn().mockImplementation(() => ({
  currentTime: mockCurrentTime,
  sampleRate: 44100,
  destination: mockDestination,
  createGain: vi.fn(() => makeMockGain()),
  createOscillator: vi.fn(() => makeMockOscillator()),
  createBiquadFilter: vi.fn(() => makeMockFilter()),
  createBuffer: vi.fn((channels, length, rate) => ({
    getChannelData: vi.fn(() => new Float32Array(length)),
  })),
  createBufferSource: vi.fn(() => makeMockBufferSource()),
}));

vi.stubGlobal('AudioContext', mockAudioContext);

const { SoundEngine, soundEngine } = await import('../src/renderer/sounds.js');

describe('SoundEngine', () => {
  let engine;

  beforeEach(() => {
    vi.clearAllMocks();
    engine = new SoundEngine();
  });

  it('initializes with correct defaults', () => {
    expect(engine.ctx).toBeNull();
    expect(engine.enabled).toBe(true);
    expect(engine.volume).toBe(0.3);
    expect(engine.toggles).toEqual({
      click: true,
      pop: true,
      swoosh: true,
      ding: true,
      tap: true,
    });
  });

  it('creates AudioContext lazily on first play', () => {
    expect(engine.ctx).toBeNull();
    engine.play('click');
    expect(mockAudioContext).toHaveBeenCalledOnce();
    expect(engine.ctx).not.toBeNull();
  });

  it('does not create AudioContext again on subsequent plays', () => {
    engine.play('click');
    engine.play('click');
    expect(mockAudioContext).toHaveBeenCalledTimes(1);
  });

  it('play("click") creates oscillator with correct frequency and duration', () => {
    engine.play('click');
    const ctx = engine.ctx;
    expect(ctx.createOscillator).toHaveBeenCalledOnce();
    const osc = ctx.createOscillator.mock.results[0].value;
    expect(osc.type).toBe('sine');
    expect(osc.frequency.value).toBe(1000);
    expect(mockOscillatorStart).toHaveBeenCalledOnce();
    expect(mockOscillatorStop).toHaveBeenCalledWith(ctx.currentTime + 0.03);
  });

  it('play("click") does nothing when enabled=false', () => {
    engine.setEnabled(false);
    engine.play('click');
    expect(mockAudioContext).not.toHaveBeenCalled();
    expect(mockOscillatorStart).not.toHaveBeenCalled();
  });

  it('play("click") does nothing when toggles.click=false', () => {
    engine.setToggle('click', false);
    engine.play('click');
    expect(mockAudioContext).not.toHaveBeenCalled();
    expect(mockOscillatorStart).not.toHaveBeenCalled();
  });

  it('setVolume updates volume and masterGain when initialized', () => {
    engine.play('click');
    const gainNode = engine.masterGain;
    engine.setVolume(0.7);
    expect(engine.volume).toBe(0.7);
    expect(gainNode.gain.value).toBe(0.7);
  });

  it('setVolume updates volume even before init', () => {
    engine.setVolume(0.5);
    expect(engine.volume).toBe(0.5);
    expect(engine.masterGain).toBeUndefined();
  });

  it('play("pop") starts and stops oscillator', () => {
    engine.play('pop');
    expect(mockOscillatorStart).toHaveBeenCalledOnce();
    expect(mockOscillatorStop).toHaveBeenCalledWith(engine.ctx.currentTime + 0.08);
  });

  it('play("swoosh") creates buffer source and starts it', () => {
    engine.play('swoosh');
    const ctx = engine.ctx;
    expect(ctx.createBuffer).toHaveBeenCalledOnce();
    expect(ctx.createBufferSource).toHaveBeenCalledOnce();
    expect(mockSourceStart).toHaveBeenCalledOnce();
  });

  it('play("ding") uses 1400Hz and 200ms duration', () => {
    engine.play('ding');
    const ctx = engine.ctx;
    const osc = ctx.createOscillator.mock.results[0].value;
    expect(osc.frequency.value).toBe(1400);
    expect(mockOscillatorStop).toHaveBeenCalledWith(ctx.currentTime + 0.2);
  });

  it('play("tap") uses 800Hz and 15ms duration', () => {
    engine.play('tap');
    const ctx = engine.ctx;
    const osc = ctx.createOscillator.mock.results[0].value;
    expect(osc.frequency.value).toBe(800);
    expect(mockOscillatorStop).toHaveBeenCalledWith(ctx.currentTime + 0.015);
  });

  it('exports a singleton soundEngine instance', () => {
    expect(soundEngine).toBeInstanceOf(SoundEngine);
  });
});
