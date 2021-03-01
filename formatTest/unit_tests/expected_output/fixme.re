[@reason.version 3.7];
/**
 * Problem: In thise example, the comment should have a space after it.
 */
let store_attributes = proc_attributes => {
  let should_write =
    /* only overwrite defined procedures */ proc_attributes.ProcAttributes.is_defined
    || not(DB.file_exists(attributes_file));
  should_write;
};
