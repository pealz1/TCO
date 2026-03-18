const COMMON_TYPOS = {
  teh: 'the',
  recieve: 'receive',
  occured: 'occurred',
  seperate: 'separate',
  definately: 'definitely',
  accomodate: 'accommodate',
  occurance: 'occurrence',
  neccessary: 'necessary',
  wierd: 'weird',
  untill: 'until',
  thier: 'their',
  alot: 'a lot',
  arguement: 'argument',
  begining: 'beginning',
  beleive: 'believe',
  calender: 'calendar',
  collegue: 'colleague',
  commitee: 'committee',
  concious: 'conscious',
  enviroment: 'environment',
  explaination: 'explanation',
  goverment: 'government',
  grammer: 'grammar',
  immediatly: 'immediately',
  independant: 'independent',
  knowlege: 'knowledge',
  mispell: 'misspell',
  noticable: 'noticeable',
  parliment: 'parliament',
  persue: 'pursue',
  publically: 'publicly',
  recomend: 'recommend',
  refrence: 'reference',
  relevent: 'relevant',
  suprise: 'surprise'
};

function checkDoubleWords(text, issues) {
  const re = /\b(\w+)\s+\1\b/gi;
  let match;
  while ((match = re.exec(text)) !== null) {
    issues.push({
      offset: match.index,
      length: match[0].length,
      message: `Repeated word "${match[1]}"`,
      suggestions: [match[1]],
      type: 'grammar'
    });
  }
}

function checkCapitalization(text, issues) {
  // Start of text
  if (text.length > 0 && /^[a-z]/.test(text)) {
    issues.push({
      offset: 0,
      length: 1,
      message: 'Sentence should start with a capital letter',
      suggestions: [text[0].toUpperCase()],
      type: 'grammar'
    });
  }

  // After period
  const re = /\.\s+([a-z])/g;
  let match;
  while ((match = re.exec(text)) !== null) {
    const charOffset = match.index + match[0].length - 1;
    issues.push({
      offset: charOffset,
      length: 1,
      message: 'Sentence should start with a capital letter',
      suggestions: [match[1].toUpperCase()],
      type: 'grammar'
    });
  }
}

function checkDoubleSpaces(text, issues) {
  const re = /  +/g;
  let match;
  while ((match = re.exec(text)) !== null) {
    issues.push({
      offset: match.index,
      length: match[0].length,
      message: 'Multiple spaces detected',
      suggestions: [' '],
      type: 'grammar'
    });
  }
}

function checkCommonTypos(text, issues) {
  const words = text.split(/\b/);
  let offset = 0;

  for (const word of words) {
    const lower = word.toLowerCase();
    if (COMMON_TYPOS[lower]) {
      issues.push({
        offset,
        length: word.length,
        message: `Possible typo: "${word}" should be "${COMMON_TYPOS[lower]}"`,
        suggestions: [COMMON_TYPOS[lower]],
        type: 'typo'
      });
    }
    offset += word.length;
  }
}

export function checkGrammar(text) {
  const issues = [];
  checkDoubleWords(text, issues);
  checkCapitalization(text, issues);
  checkDoubleSpaces(text, issues);
  checkCommonTypos(text, issues);
  // Sort by offset
  issues.sort((a, b) => a.offset - b.offset);
  return issues;
}
