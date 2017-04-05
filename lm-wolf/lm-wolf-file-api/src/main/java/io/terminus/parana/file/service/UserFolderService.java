package io.terminus.parana.file.service;

import io.terminus.common.model.Response;
import io.terminus.parana.file.model.UserFolder;

public interface UserFolderService {
   Response createFolder(UserFolder var1);

   Response updateFolder(UserFolder var1);

   Response moveFolder(Long var1, Long var2);

   Response deleteFolder(Long var1);

   Response findById(Long var1);

   Response folderPath(Long var1);

   Response pagingFiles(Long var1, Integer var2, String var3, String var4, Long var5, Integer var6, Integer var7);
}
