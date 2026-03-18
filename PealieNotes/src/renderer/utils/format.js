export function formatDate(date) {
  if (date == null) return '';

  const d = date instanceof Date ? date : new Date(date);
  if (isNaN(d.getTime())) return '';

  const now = new Date();
  const diffMs = now - d;
  const diffSec = Math.floor(diffMs / 1000);
  const diffMin = Math.floor(diffSec / 60);
  const diffHr = Math.floor(diffMin / 60);

  if (diffSec < 60) return 'Just now';
  if (diffMin < 60) return `${diffMin} min ago`;
  if (diffHr < 24) return `${diffHr} hr ago`;

  const yesterday = new Date(now);
  yesterday.setDate(yesterday.getDate() - 1);
  if (
    d.getFullYear() === yesterday.getFullYear() &&
    d.getMonth() === yesterday.getMonth() &&
    d.getDate() === yesterday.getDate()
  ) {
    return 'Yesterday';
  }

  return d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
}

const SIZE_UNITS = ['B', 'KB', 'MB', 'GB', 'TB'];

export function formatFileSize(bytes) {
  if (bytes == null || bytes < 0) return '';
  if (bytes === 0) return '0 B';

  const i = Math.min(Math.floor(Math.log10(bytes) / 3), SIZE_UNITS.length - 1);
  if (i === 0) return `${bytes} B`;

  const value = bytes / Math.pow(1000, i);
  return `${parseFloat(value.toFixed(1))} ${SIZE_UNITS[i]}`;
}

export function truncate(str, len) {
  if (str == null) return '';
  if (typeof str !== 'string') str = String(str);
  if (len <= 0) return '';
  if (str.length <= len) return str;
  return str.slice(0, len) + '...';
}
