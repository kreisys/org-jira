'use strict';
const {stdin} = process;

const getStdin = () => {
    let result = '';

    return new Promise(resolve => {
        if (stdin.isTTY) {
            resolve(result);
            return;
        }

        stdin.setEncoding('utf8');

        stdin.on('readable', () => {
            let chunk;

            while ((chunk = stdin.read())) {
                result += chunk;
            }
        });

        stdin.on('end', () => {
            resolve(result);
        });
    });
};

(async () => {
    console.log((await getStdin())
    // Ordered Lists
        .replace(/^[ \t]*(\*+)\s+/gm, function(match, stars) {
            return Array(stars.length).join("  ") + '* ';
        })
    // Un-ordered lists
        .replace(/^[ \t]*(#+)\s+/gm, function(match, nums) {
            return Array(nums.length).join("  ") + '1. ';
        })
    // Headers 1-6
        .replace(/^h([0-6])\.(.*)$/gm, function (match, level, content) {
            return Array(parseInt(level) + 1).join('#') + content;
        })
    // Bold
        .replace(/\*(\S.*)\*/g, '**$1**')
    // Italic
        .replace(/\_(\S.*)\_/g, '*$1*')
    // Monospaced text
        .replace(/\{\{([^}]+)\}\}/g, '`$1`')
    // Citations (buggy)
    //.replace(/\?\?((?:.[^?]|[^?].)+)\?\?/g, '<cite>$1</cite>')
    // Inserts
        .replace(/\+([^+]*)\+/g, '<ins>$1</ins>')
    // Superscript
        .replace(/\^([^^]*)\^/g, '<sup>$1</sup>')
    // Subscript
        .replace(/~([^~]*)~/g, '<sub>$1</sub>')
    // Strikethrough
        .replace(/(\s+)-(\S+.*?\S)-(\s+)/g, '$1~~$2~~$3')
    // Code Block
        .replace(/\{code(:([a-z]+))?([:|]?(title|borderStyle|borderColor|borderWidth|bgColor|titleBGColor)=.+?)*\}([^]*)\{code\}/gm, '```$2$5```')
    // Pre-formatted text
        .replace(/{noformat}/g, '```')
    // Un-named Links
        .replace(/\[([^|]+)\]/g, '<$1>')
    // Images
        .replace(/!(.+)!/g, '![]($1)')
    // Named Links
        .replace(/\[(.+?)\|(.+)\]/g, '[$1]($2)')
    // Single Paragraph Blockquote
        .replace(/^bq\.\s+/gm, '> ')
    // Remove color: unsupported in md
        .replace(/\{color:[^}]+\}([^]*)\{color\}/gm, '$1')
    // panel into table
        .replace(/\{panel:title=([^}]*)\}\n?([^]*?)\n?\{panel\}/gm, '\n| $1 |\n| --- |\n| $2 |')
    // table header
        .replace(/^[ \t]*((?:\|\|.*?)+\|\|)[ \t]*$/gm, function (match, headers) {
            var singleBarred =  headers.replace(/\|\|/g,'|');
            return '\n' + singleBarred + '\n' + singleBarred.replace(/\|[^|]+/g, '| --- ');
        })
    // remove leading-space of table headers and rows
        .replace(/^[ \t]*\|/gm, '|'));
})();
