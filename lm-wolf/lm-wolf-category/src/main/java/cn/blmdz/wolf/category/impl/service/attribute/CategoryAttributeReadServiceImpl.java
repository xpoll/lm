package cn.blmdz.wolf.category.impl.service.attribute;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.impl.dao.CategoryAttributeDao;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.category.service.CategoryAttributeReadService;

@Service
public class CategoryAttributeReadServiceImpl implements CategoryAttributeReadService {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributeReadServiceImpl.class);
   private final CategoryAttributeDao categoryAttributeDao;

   @Autowired
   public CategoryAttributeReadServiceImpl(CategoryAttributeDao categoryAttributeDao) {
      this.categoryAttributeDao = categoryAttributeDao;
   }

   public Response findByCategoryId(Long categoryId) {
      try {
         List<CategoryAttribute> categoryAttributes = this.categoryAttributeDao.findByCategoryId(categoryId);
         return Response.ok(categoryAttributes);
      } catch (Exception var3) {
         log.error("failed to find category attribute by categoryId={}, cause:{}", categoryId, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.attribute.find.fail");
      }
   }
}
