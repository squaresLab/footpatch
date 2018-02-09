/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import java.io.FileReader;
import java.io.IOException;
import java.io.FileNotFoundException;
//import java.io.Reader;

public class ReaderLeaks {

  public void closeResource(FileReader y) throws IOException {
    y.close();
  }

  // this proc seeds the call to closeResource, otherwise we don't see r.close()
  // in the candidate list
  public void xxx() throws IOException {
    FileReader x;
    x = new FileReader("testing.txt");
    x.close();
    closeResource(x);
  }

  //Reader  tests
  public void readerNotClosedAfterRead() throws IOException {
    FileReader r;
    r = new FileReader("testing.txt");
      r.read();
  }

}
