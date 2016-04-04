'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

import {RE_GRAMMARS} from './constants';

const AtomReason = require('../output_byte_debug_js/app.js');

const PROVIDER_NAME = 'Merlin';

module.exports = {
  name: 'atom-reason',
  grammarScopes: RE_GRAMMARS,
  scope: 'file',
  lintOnFly: true,
  lint(textEditor: atom$TextEditor) {
    const filePath = textEditor.getPath();
    if (filePath == null) {
      return [];
    }
    return new Promise((resolve, reject) => {
      const onSuccess = (diagnostics) => {resolve(diagnostics);};
      const onFailure = (err) => {reject(err);};
      AtomReason.getDiagnostics(textEditor.getBuffer().getText())(filePath)(onSuccess)(onFailure);
    });
  },
};
