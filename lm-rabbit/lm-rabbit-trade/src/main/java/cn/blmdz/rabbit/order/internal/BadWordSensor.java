package cn.blmdz.rabbit.order.internal;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.io.LineProcessor;
import com.google.common.io.Resources;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * Author:cp
 * Created on 4/26/16.
 */
@Slf4j
@Component
public class BadWordSensor {
    private static final Set<String> badWords = Sets.newHashSetWithExpectedSize(2500);

    @Autowired
    private SmallSeg smallSeg;

    @PostConstruct
    public void init() {
        try {

            Resources.readLines(Resources.getResource("dic/badword.dic"), Charsets.UTF_8, new LineProcessor<Void>() {
                @Override
                public boolean processLine(String s) throws IOException {
                    if (!Strings.isNullOrEmpty(s) && s.trim().length() > 0) {
                        badWords.add(s.trim());
                    }
                    return true;
                }

                @Override
                public Void getResult() {
                    return null;
                }
            });
        } catch (Exception e) {
            log.error("load keyword config failed, error code={}", Throwables.getStackTraceAsString(e));
        }
    }

    /**
     * 检查输入内容是否含有敏感词
     *
     * @param input 输入内容
     * @return 内容中的敏感词列表, 如果列表为空, 则表示内容合法
     */
    public List<String> filterBadWords(String input) {

        List<String> result = smallSeg.cut(input);
        List<String> filtered = Lists.newArrayList();
        for (String word : result) {
            if (badWords.contains(word)) {
                filtered.add(word);
            }
        }
        return filtered;
    }
}