#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { spawnSync, execSync } = require('child_process');

const resultStub =
    "module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result";
const projectRoot = path.join(__dirname, '..')

const walk = (dirname, fn) => {
    if (fs.statSync(dirname).isDirectory()) {
        fn(dirname);
        fs.readdirSync(dirname).forEach(child => {
            if (child.startsWith('.')) {
                return;
            }
            walk(path.join(dirname, child), fn);
        });
    }
};

const esyEcho = (sandbox, contents) => {
    return execSync(`esy ${sandbox} echo '${contents}'`, {
        cwd: projectRoot,
        encoding: 'utf8',
    }).trim();
};

const mkdirp = dir => {
    if (!fs.existsSync(dir)) {
        mkdirp(path.dirname(dir));
        fs.mkdirSync(dir);
    }
};

const getOmp = (sandbox, buildDir) => {
    const ompDir = esyEcho(
        sandbox,
        `#{@opam/ocaml-migrate-parsetree.target_dir}/_build/default/src`,
    );
    const destDir = path.join(buildDir, 'omp');
    mkdirp(destDir);
    fs.readdirSync(ompDir)
        .filter(name => name.endsWith('.pp.ml'))
        .forEach(name => {
            const destName = name.replace(/\.pp\.ml$/, '.ml');
            fs.writeFileSync(
                path.join(destDir, destName),
                fs.readFileSync(path.join(ompDir, name)),
            );
        });
    return destDir;
};

const bspackRefmt = version => {
    console.log(`Building refmt for ${version}`);
    const sandbox = '@' + version;
    execSync(`esy ${sandbox}`, {cwd: projectRoot});

    const outputDir = path.join(__dirname, 'output', version);
    mkdirp(outputDir);
    const mlFile = path.join(outputDir, 'refmt.ml');
    const byteFile = path.join(outputDir, 'refmt.byte');

    const buildDir = path.join(__dirname, 'build', version);

    const includeDirs = [];
    includeDirs.push(esyEcho(sandbox, `#{@opam/menhir.lib}/menhirLib`));
    includeDirs.push(
        esyEcho(sandbox, `#{@opam/ppx_derivers.lib}/ppx_derivers`),
    );
    includeDirs.push(getOmp(sandbox, buildDir));
    walk(path.join(esyEcho(sandbox, '#{self.target_dir}'), 'default'), dir =>
        includeDirs.push(dir),
    );

    execSync('esy')
    const bsbRes = spawnSync(
        'esy',
        [
            'bspack.exe',
            '-main-export',
            'Refmt_impl',
            '-prelude-str',
            resultStub,
            ...[].concat(...includeDirs.map(dir => ['-I', dir])),
            '-bs-MD',
            '-o',
            mlFile,
        ],
        { cwd: __dirname, encoding: 'utf8' },
    );

    if (bsbRes.status !== 0) {
        console.log('failed');
        console.log(bsbRes.stdout);
        console.log(bsbRes.stderr);
        process.exit(1);
    }

    console.log('bspacked');

    fs.writeFileSync(
        mlFile,
        fs
            .readFileSync(mlFile, 'utf8')
            .replace(/Migrate_parsetree__Ast_404/g, 'Migrate_parsetree.Ast_404')
            .replace('Ppx_derivers.derivers ()', '[]'),
    );

    const compileRes = spawnSync(
        'esy',
        [
            sandbox,
            'ocamlc',
            '-g',
            '-no-alias-deps',
            '-w',
            '-40-3',
            '-I',
            '+compiler-libs',
            'ocamlcommon.cma',
            mlFile,
            '-o',
            byteFile,
        ],
        { cwd: projectRoot, encoding: 'utf8' },
    );
    if (compileRes.status !== 0) {
        console.log(compileRes.stdout);
        console.log(compileRes.stderr);
        process.exit(1);
    }

    const result = execSync(`${byteFile}`, {
        input: 'let x=10',
        encoding: 'utf8',
    }).trim();
    if (result !== 'let x = 10;') {
        console.log(`Unexpected output from running ${byteFile}: ${result}`);
    } else {
        console.log(
            `Looks good! bspacked file:\n${mlFile}\nbyte-compiled file:\n${byteFile}`,
        );
    }
};

const getVersion = () => {
    return require('../esy.json').version
}

const setVersion = () => {
    execSync('make pre_release', {cwd: projectRoot, env: {
        ...process.ENV,
        version: getVersion()
    }})
}

setVersion();
bspackRefmt('4023');
bspackRefmt('4061');
