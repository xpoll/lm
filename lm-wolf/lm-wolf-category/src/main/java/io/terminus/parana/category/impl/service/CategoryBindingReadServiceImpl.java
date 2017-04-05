package io.terminus.parana.category.impl.service;

import com.google.common.base.Throwables;
import io.terminus.common.model.Response;
import io.terminus.parana.category.impl.dao.BackCategoryDao;
import io.terminus.parana.category.impl.dao.CategoryBindingDao;
import io.terminus.parana.category.impl.dao.FrontCategoryDao;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.model.FrontCategory;
import io.terminus.parana.category.service.CategoryBindingReadService;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CategoryBindingReadServiceImpl implements CategoryBindingReadService {
   private static final Logger log = LoggerFactory.getLogger(CategoryBindingReadServiceImpl.class);
   private final CategoryBindingDao categoryBindingDao;
   private final FrontCategoryDao frontCategoryDao;
   private final BackCategoryDao backCategoryDao;

   @Autowired
   public CategoryBindingReadServiceImpl(CategoryBindingDao categoryBindingDao, FrontCategoryDao frontCategoryDao, BackCategoryDao backCategoryDao) {
      this.categoryBindingDao = categoryBindingDao;
      this.frontCategoryDao = frontCategoryDao;
      this.backCategoryDao = backCategoryDao;
   }

   public Response findByFrontCategoryId(Long frontCategoryId) {
      try {
         List<Long> backCategoryIds = this.categoryBindingDao.findByFrontCategoryId(frontCategoryId);
         if(backCategoryIds.isEmpty()) {
            return Response.ok(Collections.emptyList());
         } else {
            List<BackCategory> backCategories = this.backCategoryDao.findByIds(backCategoryIds);
            return Response.ok(backCategories);
         }
      } catch (Exception var4) {
         log.error("failed to find  back categories by front category(id={}), cause:{}", frontCategoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.binding.find.fail");
      }
   }

   public Response findByBackCategoryId(Long backCategoryId) {
      try {
         List<Long> frontCategoryIds = this.categoryBindingDao.findByBackCategoryId(backCategoryId);
         if(frontCategoryIds.isEmpty()) {
            return Response.ok(Collections.emptyList());
         } else {
            List<FrontCategory> frontCategories = this.frontCategoryDao.findByIds(frontCategoryIds);
            return Response.ok(frontCategories);
         }
      } catch (Exception var4) {
         log.error("failed to find  front categories by back category(id={}), cause:{}", backCategoryId, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.binding.find.fail");
      }
   }
}
