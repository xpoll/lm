package cn.blmdz.wolf.file.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.file.model.UserFile;

public interface UserFileService {
   Response createFile(UserFile var1);

   Response updateFile(UserFile var1);

   Response findById(Long var1);

   Response moveFile(Long var1, Long var2);

   Response deleteFile(Long var1);
}
