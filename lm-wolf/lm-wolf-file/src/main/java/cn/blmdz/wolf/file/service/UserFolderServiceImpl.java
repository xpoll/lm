package cn.blmdz.wolf.file.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Optional;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.file.dao.UserFileDao;
import cn.blmdz.wolf.file.dao.UserFolderDao;
import cn.blmdz.wolf.file.dto.FilePagingDto;
import cn.blmdz.wolf.file.dto.FileRealPath;
import cn.blmdz.wolf.file.dto.UserFileDto;
import cn.blmdz.wolf.file.model.UserFile;
import cn.blmdz.wolf.file.model.UserFolder;
import cn.blmdz.wolf.file.service.UserFolderService;

@Service
public class UserFolderServiceImpl implements UserFolderService {
   private static final Logger log = LoggerFactory.getLogger(UserFolderServiceImpl.class);
   @Autowired
   private UserFolderDao userFolderDao;
   @Autowired
   private UserFileDao userFileDao;

   public Response createFolder(UserFolder folder) {
      Response<Long> result = new Response();

      try {
         folder.setFolder(this.newFolderName(folder.getPid(), folder.getFolder(), Integer.valueOf(0)));
         int level = 1;
         if(folder.getPid().longValue() != 0L) {
            UserFolder parentFolder = (UserFolder)this.userFolderDao.findById(folder.getPid());
            if(parentFolder == null) {
               log.error("Can\'t find parent folder by id={}", folder.getId());
               result.setError("user.folder.id.invalid");
               return result;
            }

            level = parentFolder.getLevel().intValue() + 1;
         }

         folder.setLevel(Integer.valueOf(level));
         this.userFolderDao.create(folder);
         result.setResult(folder.getId());
      } catch (Exception var5) {
         log.error("Failed to create folder({}), cause:{}", folder, Throwables.getStackTraceAsString(var5));
         result.setError("folder.create.fail");
      }

      return result;
   }

   public Response updateFolder(UserFolder folder) {
      Response<Boolean> result = new Response();

      try {
         UserFolder oldFolder = (UserFolder)this.userFolderDao.findById(folder.getId());
         if(oldFolder == null) {
            log.error("Failed query folder by id={}", folder.getId());
            result.setError("folder.find.failed");
            return result;
         }

         if(!Objects.equals(oldFolder.getFolder(), folder.getFolder())) {
            folder.setFolder(this.newFolderName(folder.getPid(), folder.getFolder(), Integer.valueOf(0)));
         }

         result.setResult(this.userFolderDao.update(folder));
      } catch (Exception var4) {
         log.error("Failed to update folder({}), cause:{}", folder, Throwables.getStackTraceAsString(var4));
         result.setError("folder.update.fail");
      }

      return result;
   }

   public Response moveFolder(Long fid, Long newPid) {
      Response<Boolean> result = new Response();

      try {
         UserFolder folder = (UserFolder)this.userFolderDao.findById(fid);
         if(folder == null) {
            log.error("Failed query folder by id={}", fid);
            result.setError("folder.find.failed");
            return result;
         }

         UserFolder pFolder = (UserFolder)this.userFolderDao.findById(newPid);
         if(newPid.longValue() != 0L && pFolder == null) {
            log.error("Failed query folder by id={}", newPid);
            result.setError("folder.find.failed");
            return result;
         }

         folder.setPid(newPid);
         folder.setLevel(Integer.valueOf(pFolder.getLevel().intValue() + 1));
         result.setResult(this.userFolderDao.update(folder));
      } catch (Exception var6) {
         log.error("Move folder failed, fid={}, newPid={}, cause:{}", new Object[]{fid, newPid, Throwables.getStackTraceAsString(var6)});
         result.setError("folder.move.failed");
      }

      return result;
   }

   public Response deleteFolder(Long folderId) {
      Response<UserFolder> result = new Response();

      try {
         UserFolder folder = (UserFolder)this.userFolderDao.findById(folderId);
         if(folder == null) {
            log.error("Failed query folder by id={}", folderId);
            result.setError("folder.find.failed");
            return result;
         }

         this.userFolderDao.delete(folderId);
         result.setResult(folder);
      } catch (Exception var4) {
         log.error("Delete folder failed, fid={}, cause:{}", folderId, Throwables.getStackTraceAsString(var4));
         result.setError("folder.delete.failed");
      }

      return result;
   }

   public Response findById(Long folderId) {
      Response<Optional<UserFolder>> result = new Response();

      try {
         UserFolder folder = (UserFolder)this.userFolderDao.findById(folderId);
         result.setResult(Optional.fromNullable(folder));
      } catch (Exception var4) {
         log.error("Find folder failed, fid={}", folderId, Throwables.getStackTraceAsString(var4));
         result.setError("folder.find.failed");
      }

      return result;
   }

   public Response folderPath(Long folderId) {
      Response<FileRealPath> result = new Response();

      try {
         List<UserFolder> userFolders = Lists.newArrayList();
         this.realPath(userFolders, folderId);
         Collections.reverse(userFolders);
         String realPath = "/";

         for(UserFolder userFolder : userFolders) {
            realPath = realPath + userFolder.getFolder();
         }

         FileRealPath fileRealPath = new FileRealPath();
         fileRealPath.setRealPath(realPath);
         fileRealPath.setPathList(userFolders);
         result.setResult(fileRealPath);
      } catch (Exception var7) {
         log.error("Failed query folder path, folderId={}", folderId);
         result.setError("folder.find.fail");
      }

      return result;
   }

   private void realPath(List folders, Long folderId) {
      UserFolder userFolder = (UserFolder)this.userFolderDao.findById(folderId);
      if(userFolder != null) {
         folders.add(userFolder);
         this.realPath(folders, userFolder.getPid());
      }

   }

   public Response pagingFiles(Long userId, Integer type, String group, String fileName, Long folderId, Integer pageNo, Integer pageSize) {
      Response<FilePagingDto> result = new Response();

      try {
         Map<String, Object> params = Maps.newHashMap();
         params.put("fileType", type);
         params.put("pid", folderId);
         params.put("name", fileName);
         params.put("createBy", userId);
         PageInfo page = new PageInfo(pageNo, pageSize);
         Integer offset = page.getOffset();
         Integer limit = page.getLimit();
         List<UserFileDto> userFileDtoList = Lists.newArrayList();
         Paging<UserFolder> folderPaging = this.userFolderDao.paging(offset, limit, params);
         Long allTotal = folderPaging.getTotal();

         for(UserFolder userFolder : folderPaging.getData()) {
            UserFileDto userFileDto = new UserFileDto();
            userFileDto.setFolder(userFolder);
            userFileDtoList.add(userFileDto);
         }

         int newOffset = 0;
         int newLimit = 0;
         if(folderPaging.getTotal().longValue() < (long)(offset.intValue() + limit.intValue())) {
            int foldOffset = offset.intValue() + limit.intValue() - folderPaging.getTotal().intValue();
            newLimit = foldOffset;
            if(foldOffset > limit.intValue()) {
               newOffset = foldOffset - limit.intValue();
               newLimit = limit.intValue();
            }
         }

         params.put("folderId", folderId);
         Paging<UserFile> imagePaging = this.userFileDao.paging(Integer.valueOf(newOffset), Integer.valueOf(newLimit), params);
         allTotal = Long.valueOf(allTotal.longValue() + imagePaging.getTotal().longValue());

         for(UserFile userFile : imagePaging.getData()) {
            UserFileDto userFileDto = new UserFileDto();
            userFileDto.setUserFile(userFile);
            userFileDtoList.add(userFileDto);
         }

         FilePagingDto filePagingDto = new FilePagingDto();
         filePagingDto.setFileDtoPaging(new Paging(allTotal, userFileDtoList));
         Response<FileRealPath> pathRes = this.folderPath(folderId);
         if(pathRes.isSuccess()) {
            filePagingDto.setFileRealPath((FileRealPath)pathRes.getResult());
         }

         result.setResult(filePagingDto);
      } catch (Exception var22) {
         log.error("Failed find folder & file(userId={}, type={}, folderId={}, pageNo={}, pageSize={}), cause: {}", new Object[]{userId, type, folderId, pageNo, pageSize, Throwables.getStackTraceAsString(var22)});
         result.setError("user.image.find.fail");
      }

      return result;
   }

   private String newFolderName(Long pid, String folder, Integer index) {
      String newFolder = index.intValue() == 0?folder:folder + "(" + index + ")";
      List<UserFolder> folders = this.userFolderDao.list(ImmutableMap.of("pid", pid, "folder", newFolder));
      return folders.isEmpty()?newFolder:this.newFolderName(pid, folder, Integer.valueOf(index.intValue() + 1));
   }
}
