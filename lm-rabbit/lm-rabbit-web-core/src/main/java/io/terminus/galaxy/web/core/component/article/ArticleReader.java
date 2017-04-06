package io.terminus.galaxy.web.core.component.article;

import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Lists;
import io.terminus.common.model.Response;
import io.terminus.galaxy.web.core.enums.ArticleType;
import io.terminus.pampas.client.Export;
import io.terminus.parana.article.model.Article;
import io.terminus.parana.article.service.ArticleReadService;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Nullable;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Date: 6/23/16
 * Time: 7:10 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Component
public class ArticleReader {
    private final ArticleReadService articleReadService;

    @Autowired
    public ArticleReader(ArticleReadService articleReadService) {
        this.articleReadService = articleReadService;
    }

    @Export
    public Response<List<RichTypeArticle>> listTypeArticle() {
        try {
            // 获取全部文章
            Response<List<Article>> findAll = articleReadService.listBy(null);
            if (!findAll.isSuccess()) {
                log.error("fail to find all article, cause:{}", findAll.getError());
                return Response.fail(findAll.getError());
            }

            if (findAll.getResult().isEmpty()) {
                return Response.ok(Collections.<RichTypeArticle>emptyList());
            }

            // 按照类型归组
            ImmutableMultimap<Integer, Article> typeToArticles = FluentIterable.from(findAll.getResult())
                    .index(new Function<Article, Integer>() {
                        @Nullable
                        @Override
                        public Integer apply(@Nullable Article input) {
                            return input.getType();
                        }
                    });

            // 组装数据
            List<RichTypeArticle> richTypeArticles = Lists.newArrayList();
            for (ArticleType type: ArticleType.values()) {
                final Integer t = type.value();
                RichTypeArticle richTypeArticle = new RichTypeArticle();
                richTypeArticle.setType(t);
                richTypeArticle.setDesc(type.toString());

                if (typeToArticles.containsKey(t)) {
                    richTypeArticle.setArticles(new ArrayList<>(typeToArticles.get(t)));
                } else {
                    richTypeArticle.setArticles(Collections.<Article>emptyList());
                }

                richTypeArticles.add(richTypeArticle);
            }

            return Response.ok(richTypeArticles);
        } catch (Exception e) {
            log.error("fail , cause:{}", Throwables.getStackTraceAsString(e));
            return Response.fail(e.getMessage());
        }
    }

    /**
     * 按照分类归组的文章
     */
    @Data
    private class RichTypeArticle implements Serializable {
        private static final long serialVersionUID = 9214819884259515902L;

        Integer type;
        String desc;

        List<Article> articles;
    }
}
