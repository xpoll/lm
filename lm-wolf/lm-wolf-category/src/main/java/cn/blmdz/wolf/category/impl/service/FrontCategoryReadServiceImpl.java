package cn.blmdz.wolf.category.impl.service;

import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.FrontCategoryDao;
import cn.blmdz.wolf.parana.category.model.FrontCategory;
import cn.blmdz.wolf.parana.category.service.FrontCategoryReadService;

@Service
public class FrontCategoryReadServiceImpl implements FrontCategoryReadService {
   private static final Logger log = LoggerFactory.getLogger(FrontCategoryReadServiceImpl.class);
   @Autowired
   private FrontCategoryDao frontCategoryDao;

   public Response findChildrenByPid(Long pid) {
      pid = (Long)MoreObjects.firstNonNull(pid, Long.valueOf(0L));

      try {
         return Response.ok(this.frontCategoryDao.findChildren(pid));
      } catch (Exception var3) {
         log.error("failed to find children of front category(id={}), cause:{}", pid, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.find.fail");
      }
   }

   public Response findById(Long id) {
      try {
         FrontCategory frontCategory = (FrontCategory)this.frontCategoryDao.findById(id);
         if(frontCategory == null) {
            log.error("no front category(categoryId={}) found", id);
            return Response.fail("frontCategory.not.found");
         } else {
            return Response.ok(frontCategory);
         }
      } catch (Exception var3) {
         log.error("failed to find front category(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.find.fail");
      }
   }

   public Response findByIds(List ids) {
      if(CollectionUtils.isEmpty(ids)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<FrontCategory> frontCategory = this.frontCategoryDao.findByIds(ids);
            return Response.ok(frontCategory);
         } catch (Exception var3) {
            log.error("failed to find front categories(ids={}), cause:{}", ids, Throwables.getStackTraceAsString(var3));
            return Response.fail("category.find.fail");
         }
      }
   }
}
