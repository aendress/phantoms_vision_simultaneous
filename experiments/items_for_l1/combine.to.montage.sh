#!/bin/sh

../create_items_for_l1.pl 

/usr/local/bin/montage word_set1_h_new* -trim -tile 1x6 m_word_set1_h.bmp
/usr/local/bin/montage word_set1_v_new* -trim -tile 6x1 m_word_set1_v.bmp

/usr/local/bin/montage word_set2_h_new* -trim -tile 1x6 m_word_set2_h.bmp
/usr/local/bin/montage word_set2_v_new* -trim -tile 6x1 m_word_set2_v.bmp

/usr/local/bin/montage phantom_set1_h_new* -trim -tile 1x2 m_phantom_set1_h.bmp
/usr/local/bin/montage phantom_set1_v_new* -trim -tile 2x1 m_phantom_set1_v.bmp

/usr/local/bin/montage phantom_set2_h_new* -trim -tile 1x2 m_phantom_set2_h.bmp
/usr/local/bin/montage phantom_set2_v_new* -trim -tile 2x1 m_phantom_set2_v.bmp
