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
    var map = {
      //cite: '??',
      del: '-',
      ins: '+',
      sup: '^',
      sub: '~'
    };

    console.log((await getStdin())
       .replace(/^\n((?:\|.*?)+\|)[ \t]*\n((?:\|\s*?\-{3,}\s*?)+\|)[ \t]*\n((?:(?:\|.*?)+\|[ \t]*\n)*)$/gm,
        function (match, headerLine, separatorLine, rowstr) {
            var headers = headerLine.match(/[^|]+(?=\|)/g);
            var separators = separatorLine.match(/[^|]+(?=\|)/g);
            if (headers.length !== separators.length) {
                return match;
            }
            var rows = rowstr.split('\n');
            if (rows.length === 1 + 1 && headers.length === 1) {
                // panel
                return '{panel:title=' + headers[0].trim() + '}\n' +
                    rowstr.replace(/^\|(.*)[ \t]*\|/, '$1').trim() +
                    '\n{panel}\n';
            } else {
                return '||' + headers.join('||') + '||\n' + rowstr;
            }
        })
        // Bold, Italic, and Combined (bold+italic)
        .replace(/([*_]+)(\S.*?)\1/g, function (match,wrapper,content) {
            switch (wrapper.length) {
                case 1: return '_' + content + '_';
                case 2: return '*' + content + '*';
                case 3: return '_*' + content + '*_';
                default: return wrapper + content * wrapper;
            }
         })
         // All Headers (# format)
         .replace(/^([#]+)(.*?)$/gm, function (match,level,content) {
             return 'h' + level.length + '.' + content;
         })
         // Headers (H1 and H2 underlines)
         .replace(/^(.*?)\n([=-]+)$/gm, function (match,content,level) {
             return 'h' + (level[0] === '=' ? 1 : 2) + '. ' + content;
         })
        // Ordered lists
        .replace(/^([ \t]*)\d+\.\s+/gm, function(match, spaces) {
            return Array(Math.floor(spaces.length/2 + 1)).join("#") + '# ';
        })
        // Un-Ordered Lists
        .replace(/^([ \t]*)\*\s+/gm, function(match, spaces) {
            return Array(Math.floor(spaces.length/2 + 1)).join("*") + '* ';
        })
        // Headers (h1 or h2) (lines "underlined" by ---- or =====)
        // Citations, Inserts, Subscripts, Superscripts, and Strikethroughs
        .replace(new RegExp('<(' + Object.keys(map).join('|') + ')>(.*?)<\/\\1>', 'g'), function (match,from,content) {
            var to = map[from];
            return to + content + to;
        })
        // Other kind of strikethrough
        .replace(/(\s+)~~(.*?)~~(\s+)/g, '$1-$2-$3')
        // Named/Un-Named Code Block
        .replace(/```(.+\n)?((?:.|\n)*?)```/g, function(match, synt, content) {
            var code = '{code}';
            if (synt) {
                code = '{code:' + synt.replace(/\n/g, '') + "}\n";
            }
            return code + content + '{code}';
        })
        // Inline-Preformatted Text
        .replace(/`([^`]+)`/g, '{{$1}}')
        // Images
        .replace(/!\[[^\]]*\]\(([^)]+)\)/g, '!$1!')
        // Named Link
        .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '[$1|$2]')
        // Un-Named Link
        .replace(/<([^>]+)>/g, '[$1]')
        // Single Paragraph Blockquote
        .replace(/^>/gm, 'bq.'));
})();
