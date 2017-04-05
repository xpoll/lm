package io.terminus.parana.file.service;

import com.google.common.base.Optional;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.model.Response;
import io.terminus.parana.file.dao.UserFileDao;
import io.terminus.parana.file.dao.UserFolderDao;
import io.terminus.parana.file.model.UserFile;
import io.terminus.parana.file.model.UserFolder;
import io.terminus.parana.file.service.UserFileService;
import io.terminus.parana.file.util.FileUtil;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserFileServiceImpl implements UserFileService {
   private static final Logger log = LoggerFactory.getLogger(UserFileServiceImpl.class);
   @Autowired
   private UserFileDao userFileDao;
   @Autowired
   private UserFolderDao userFolderDao;

   public Response createFile(UserFile userFile) {
      Response<Long> result = new Response();

      try {
         UserFolder folder = (UserFolder)this.userFolderDao.findById(userFile.getFolderId());
         if(userFile.getFolderId().longValue() != 0L && folder == null) {
            log.error("Failed query folder by id={}", userFile.getFolderId());
            result.setError("folder.find.failed");
            return result;
         }

         userFile.setName(this.newFileName(userFile.getFolderId(), userFile.getName(), Integer.valueOf(0)));
         userFile.setFileType(Integer.valueOf(FileUtil.fileType(userFile.getName()).value()));
         this.userFileDao.create(userFile);
         result.setResult(userFile.getId());
      } catch (Exception var4) {
         log.error("Filed to create file({}), cause: {}", userFile, Throwables.getStackTraceAsString(var4));
         result.setError("file.create.failed");
      }

      return result;
   }

   public Response updateFile(UserFile userFile) {
      Response<Boolean> result = new Response();

      try {
         UserFile oldFile = (UserFile)this.userFileDao.findById(userFile.getId());
         if(oldFile == null) {
            log.error("Failed query file by id={}", userFile.getId());
            result.setError("file.find.failed");
            return result;
         }

         if(!Objects.equals(oldFile.getName(), oldFile.getName())) {
            userFile.setName(this.newFileName(userFile.getFolderId(), userFile.getName(), Integer.valueOf(0)));
         }

         result.setResult(this.userFileDao.update(userFile));
      } catch (Exception var4) {
         log.error("Failed to update file({}), cause:{}", userFile, Throwables.getStackTraceAsString(var4));
         result.setError("file.update.failed");
      }

      return result;
   }

   public Response findById(Long fileId) {
      Response<Optional<UserFile>> result = new Response();

      try {
         UserFile userFile = (UserFile)this.userFileDao.findById(fileId);
         result.setResult(Optional.fromNullable(userFile));
      } catch (Exception var4) {
         log.error("Find file failed, fileId={}", fileId, Throwables.getStackTraceAsString(var4));
         result.setError("file.find.failed");
      }

      return result;
   }

   public Response moveFile(Long fileId, Long folderId) {
      Response<Boolean> result = new Response();

      try {
         UserFile userFile = (UserFile)this.userFileDao.findById(fileId);
         if(userFile == null) {
            log.error("Failed query file by id={}", fileId);
            result.setError("file.find.failed");
            return result;
         }

         UserFolder folder = (UserFolder)this.userFolderDao.findById(folderId);
         if(userFile.getFolderId().longValue() != 0L && folder == null) {
            log.error("Failed query folder by id={}", folderId);
            result.setError("folder.find.failed");
            return result;
         }

         result.setResult(this.userFileDao.update(userFile));
      } catch (Exception var6) {
         log.error("Move file to folder failed, fileId={}, folderId={}, cause:{}", new Object[]{fileId, folderId, Throwables.getStackTraceAsString(var6)});
         result.setError("file.move.failed");
      }

      return result;
   }

   public Response deleteFile(Long fileId) {
      Response<UserFile> result = new Response();

      try {
         UserFile userFile = (UserFile)this.userFileDao.findById(fileId);
         if(userFile == null) {
            log.error("Failed query file by id={}", fileId);
            result.setError("file.find.failed");
            return result;
         }

         this.userFileDao.delete(fileId);
         result.setResult(userFile);
      } catch (Exception var4) {
         log.error("Delete file failed, fid={}, cause:{}", fileId, Throwables.getStackTraceAsString(var4));
         result.setError("file.delete.failed");
      }

      return result;
   }

   private String newFileName(Long folderId, String fileName, Integer index) {
      String newName = index.intValue() == 0?fileName:fileName + "(" + index + ")";
      List<UserFile> files = this.userFileDao.list(ImmutableMap.of("folderId", folderId, "name", newName));
      return files.isEmpty()?newName:this.newFileName(folderId, fileName, Integer.valueOf(index.intValue() + 1));
   }
}
