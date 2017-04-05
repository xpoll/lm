package io.terminus.parana.file.service;

import io.terminus.common.model.Response;
import io.terminus.parana.file.model.UserFile;

public interface UserFileService {
   Response createFile(UserFile var1);

   Response updateFile(UserFile var1);

   Response findById(Long var1);

   Response moveFile(Long var1, Long var2);

   Response deleteFile(Long var1);
}
